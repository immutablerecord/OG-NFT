const fetch = require("node-fetch");
const _ =  require("lodash");
const Pact = require("pact-lang-api");
const SigData =  require('./Pact.SigBuilder.js');

const eventHost = "testnetqueries.kadena.network";
const limit = 100;
const offset = 300;

const sortEvents = (ev1, ev2, newestToOldest=false) => {
  return newestToOldest ? ev2.height-ev1.height : ev1.height-ev2.height;
};

/** takes a pactRep and makes it more friendly */
const convertPactRep = (val) => {
  if (typeof val === 'object' && val !== null) {
    const ks = Object.keys(val);
    if (ks.length > 1) {
      return val;
    } else if (ks[0] === "int" || ks[0] === "decimal") {
      if (typeof val[ks[0]] === 'number') { return val[ks[0]].toString(); }
      else { return val[ks[0]]}
    } else if (ks[0] === "time" || ks[0] === "timep") {
      return val[ks[0]];
    } else {
      throw new Error(`Pact base type converstion match failed: ${val}`);
    }
  } else { return val; }
}

const parseEventParams = (convertParams, {name, params, ...rest}) => {
  return {
    name,
    params: convertParams(name, params),
    ...rest
  }
};

const getCWDataEvents = async (name, offset, limit=50) => {
  console.debug(`fetching ${name} events`, {limit, offset})
  const raw = fetch(`https://${eventHost}/txs/events?name=${name}\&limit\=${limit}\&offset\=${offset}`);
  const rawRes = await raw;
  const res = await rawRes;
  if (res.ok){
     const resJSON = await rawRes.json();
     return resJSON
   } else {
     const resTEXT = await rawRes.text();
     return resTEXT;
   }
};

const syncEventsFromCWData = async (name, limit=50, threads=4, newestToOldest=false, moduleHashBlacklist=[]) => {
  console.debug(`starting to get ${name} events`)
  var offset = 0;
  var promisedResults = [];
  var completedResults = [];
  var continueSync = true;
  while (continueSync) {
    console.debug(`${name} events, doing batch`, {offset, limit, threads});
    for (var i = 0; i < threads; i++) {
      promisedResults.push(getCWDataEvents(name, offset, limit));
      offset = (offset + limit);
    };
    completedResults = await Promise.all(promisedResults);
    // once a batch comes back empty, we're caught up
    continueSync = _.every(_.map(completedResults, (v) => v.length >= limit));
  };
  completedResults = _.filter(_.flatten(completedResults), ({moduleHash}) => {return !moduleHashBlacklist.includes(moduleHash);});
  const stateObj = _.map(completedResults, (ev) => parseEventParams(convertOGParams, ev)).sort((a,b)=>sortEvents(a,b,newestToOldest));
  // console.debug(`${name} events`, stateObj);
  return stateObj;
};

const convertOGParams = (name, params) => {
  const ps = _.map(params, convertPactRep);
  switch (name) {
    case `user.policy1.UPDATE_OWNER`:
      return _.zipObject(["id", "account", "guard", "weight"], ps);
    case `user.policy1.QUOTE`:
      return _.zipObject(["saleId", "tokenId", "recipient", "amount", "price", "salePrice"], ps);
    case `user.policy1.TOKEN_WEIGHT`:
      return _.zipObject(["tokenId", "weight"], ps);
    case `marmalade.ledger.SALE`:
      return _.zipObject(["tokenId", "seller", "amount", "timeout", "saleId"], ps);
    default:
      throw new Error(`Event converstion match failed: ${name} -- ${JSON.stringify(ps)}`);
  }
};

const key = (token, acct) => `${token}:${acct}`

const updateOwners = (evts) => {
  const owners = {};
  evts.forEach(ev => {
    if (ev.name === "user.policy1.UPDATE_OWNER") {
      const acctName = key(ev.params.id, ev.params.account);
      owners[acctName] = {
        id: ev.params.id,
        account: ev.params.account,
        weight: parseInt(ev.params.weight)
      };
    }
  })
  return owners
}

const updateQuotes = (evts) => evts.filter(ev => ev.name === "user.policy1.QUOTE");
const updateSales = (evts) => evts.filter(ev => ev.name === "marmalade.ledger.SALE");


// Functions to create Buy Commands

const calculateRoyaltyPayout = (tokenId, seller, amount, salePrice, owners) => {
  let royaltyShare = salePrice * 0.1;
  let totalWeight = Object.values(owners).reduce((a,b) =>{ return a + b.weight }, 0)
  let payouts = {};
  for (owner in owners) {
    const {id, account, weight} = owners[owner];
    if (weight > 0){
      if (tokenId === id && seller === account) {
        const sellerWeight = weight - amount;
        if (payouts.hasOwnProperty(account)){
          payouts[account] = {
            "account": account,
            "payout": payouts[account].payout + truncate(royaltyShare * sellerWeight / totalWeight, 12)
          }
        } else {
          payouts[account] = {
            "account": account,
            "payout": truncate(royaltyShare * sellerWeight / totalWeight, 12)
          }
        }
      }
      else {
        if (payouts.hasOwnProperty(account)){
          payouts[account] = {
            "account": account,
            "payout": payouts[account].payout + truncate(royaltyShare * weight / totalWeight, 12)
          }
        } else {
          payouts[account] = {
            "account": account,
            "payout": truncate(royaltyShare * weight / totalWeight, 12)
          }
        }
      }
    }
  }
  return payouts;
}

function truncate(v, p) {
    var s = Math.pow(10, p || 0);
    return Math.trunc(s * v) / s;
}

const addBuyCaps = (quoteParams, saleParams, buyer, owners) => { // owners is the returned value from updatedOwners();
  const {saleId, tokenId, amount, salePrice, recipient} = quoteParams;
  const {seller, timeout} = saleParams;
  const payouts = calculateRoyaltyPayout(tokenId, seller, amount, salePrice, owners);

  // Add BUY cap
  let caps = [];
  caps.push({
    "name": "marmalade.ledger.BUY",
    "args": [tokenId, seller, buyer, amount, {int: timeout}, saleId]
  })
  // Add coin.TRANSFER cap
  Object.values(payouts).forEach(receiver => {
    const {account, payout} = receiver;
    if (account === recipient) {
      caps.push({
        "name": "coin.TRANSFER",
        "args": [buyer, account, payout + truncate(salePrice * 0.9, 12)]
      })
    } else {
      caps.push({
        "name": "coin.TRANSFER",
        "args": [buyer, account, payout]
      })
    }
  })
  return caps;
}

const signBuyCommand = (
  quoteParams,
  owners,
  sender,
  gasSigningKey,
  buySigningKey,
  networkId,
  chainId,
  gasPrice,
  gasLimit,
  envData={}, caps=[]
) => {
  const {
      saleId
    , tokenId
    , recipient
    , amount
    , price
    , salePrice
  } = quoteParams;

  const meta = Pact.lang.mkMeta(sender, chainId, Number.parseFloat(gasPrice), Number.parseFloat(gasLimit), SigData.util.autoCreationTime(), 15000);
  const gasPayerCap = SigData.mkSignerCList(gasSigningKey, SigData.util.addGasCap([]));
  const buyerCap = SigData.mkSignerCList(buySigningKey, caps);
  const signers =gasPayerCap.concat(buyerCap);
  const cmdJSON = SigData.mkContPayload(
    saleId,
    1,
    signers,
    networkId,
    meta,
    { data: envData,
      rollback: false}
  );
  const execSigData = SigData.mkSigData(cmdJSON);
  return execSigData;
};

//Sample Buy

  const sampleSale = {
    name: 'marmalade.ledger.SALE',
    params: {
      tokenId: 'policy1-test-token-5',
      seller: 'k:dd13cce5db0e6ccc12fd55e2acff35aa559b13ac5a5c2f2eaf5589a6dc809a9f',
      amount: 5,
      timeout: '2619077',
      saleId: 'aRFKqj1gWn9Fa59rVYm1nzn0vIAfZhyOzwWE8VaQaRk'
    },
    blockTime: '2022-04-26T15:56:03.639491Z',
    height: 2149181,
    blockHash: 'xYlyyQgHeV35GLUwn15nPS--llFD_fegcRXPhJXP_SQ',
    requestKey: 'aRFKqj1gWn9Fa59rVYm1nzn0vIAfZhyOzwWE8VaQaRk',
    idx: 2,
    chain: 1,
    moduleHash: 'ovxYn-4UNKoLxKFgxccjPM076lqZfuD3H89qvzqd0t0'
  }

const gasPrice = 0.00000001;
const gasLimit = 80000;
const networkId = "testnet04";
const chainId = "1"

const gasPayer = "k:1af2ce66f81d799d7850c23e6729eb47d3e400666401edba8acce0cd5692d733";
const gasPayerPubKey = "1af2ce66f81d799d7850c23e6729eb47d3e400666401edba8acce0cd5692d733";

const buyer = "k:d8319985f19a8e02ec60b125fa0b140f8f614738b3c8d613fbcd6949c59eb948";
const buyerPubKey = "d8319985f19a8e02ec60b125fa0b140f8f614738b3c8d613fbcd6949c59eb948";
const buyerGuard = {
  keys: [buyerPubKey],
  pred: "keys-all"
}

const printBuySigData = (buyParams) => {
  const {
     sampleSale
   , gasPrice
   , gasLimit
   , networkId
   , chainId
   , gasPayer
   , gasPayerPubKey
   , buyer
   , buyerPubKey
   , buyerGuard } = buyParams;

  syncEventsFromCWData("user.policy1").then(stateObj => {

    const quote = updateQuotes(stateObj).filter(q=> q.params.saleId === sampleSale.params.saleId)[0];
    const owners = updateOwners(stateObj);

    console.log(
      "Buyer Command SigData: ",
      JSON.stringify(signBuyCommand(
      quote.params,
      owners,
      gasPayer,
      gasPayerPubKey,
      buyerPubKey,
      networkId, chainId, gasPrice, gasLimit,
       { "buyer": buyer,
         "buyer-guard": buyerGuard,
       },
       addBuyCaps(quote.params, sampleSale.params, buyer, owners)
      )
      )
    )
  });
}

printBuySigData({
    sampleSale
  , gasPrice
  , gasLimit
  , networkId
  , chainId
  , gasPayer
  , gasPayerPubKey
  , buyer
  , buyerPubKey
  , buyerGuard
});

