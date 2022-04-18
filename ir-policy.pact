(namespace (read-msg 'ns))

(module policy GOVERNANCE

  @doc "Immutable Record token policy."

  (defcap GOVERNANCE ()
    (enforce-keyset 'ir-admin))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema ir-owner
    id:string
    owner:string
    guard:guard
    weight:integer
  )

  (defschema ir-token
    id:string
    total-weight:integer
    curr-owned-weight:integer
    owners:[object{ir-owner}]
  )

  (deftable ir-tokens:{ir-token})

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      recipient:string
      amount:decimal
      price:decimal
      sale-price:decimal
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defconst ROYALTY 0.1
    "Share divided amongst collection owners")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})

  (defcap UPDATE_OWNER (id:string owner:string guard:guard new-weight:integer)
    @event true)

  (defcap INTERNAL () true)

  (defcap CREATE_TOKEN (id:string)
    (enforce-ledger)
    (enforce-keyset 'ir-admin)
  )

  (defcap TOKEN_WEIGHT (id:string weight:integer)
    @event true
  )

  (defcap MINT (id:string)
    (enforce-ledger)
    (enforce-keyset 'ir-admin)
  )

  (defun key ( id:string account:string ) ;; rename to account-key
    (format "{}:{}" [id account])
  )

  (defun get-policy:object{ir-token} (token:object{token-info})
    (read ir-tokens (at 'id token))
  )

  (defun get-owners:object{ir-owner} (token:object{token-info})
    (at 'owners (get-policy token))
  )

  (defun get-owner:list (account:string owners:[object{ir-owner}])
    (filter (is-owner account) owners)
  )

  (defun get-owner-weight:integer (account:string owners:[object{ir-owner}])
    (let ((owner (get-owner account owners)))
      (if (!= owner [])
        (at 'weight (at 0 owner))
        0)))

  (defun is-owner:bool (account:string owner:object{ir-owner})
    (= account (at 'owner owner))
  )

  (defun not-owner:bool (account:string owner:object{ir-owner})
    (!= account (at 'owner owner))
  )

  (defun not-partial-owner:bool (token:object{token-info} account:string)
    (let* ( (bal:integer (try 0 (floor (marmalade.ledger.get-balance (at 'id token) account))))
            (owners:list (get-owners token))
            (weight:integer (get-owner-weight account owners)))
      (= weight bal))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (with-capability (CREATE_TOKEN (at 'id token))
      (let* ( (weight:integer (read-integer 'weight ))
              (id:string (at 'id token))
              (precision:integer (at 'precision token))
            )
        (enforce (> weight 0) "Invalid weight")
        (enforce (= precision 0) "Invalid precision")
        (insert ir-tokens id
          { "id" : id,
            "total-weight" : weight,
            "curr-owned-weight": 0,
            "owners" : []
          })
        (emit-event (TOKEN_WEIGHT id weight))
        true)
      )
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (with-capability (MINT (at 'id token))
      (let*
        ( (policy (get-policy token)))
        (enforce (= (at 'supply token) 0.0) "enforce 1-off supply")
        (enforce (= amount (+ 0.0 (at 'total-weight policy))) "enforce 1-off amount") ;; look for a better way to convert weight into decimal
        (with-capability (INTERNAL) (update-owner token account guard 0.0))
      ))
  )

  (defun update-owner (token:object{token-info} account:string guard:guard amount:decimal) ;integer
    (require-capability (INTERNAL))
    (if (!= amount 0.0)
      (let ((coin-guard:guard (at 'guard (coin.details account))))
        (enforce (= coin-guard guard) "Account guard does not match")
      )
    true)
    (with-read ir-tokens (at 'id token) {
        "id" := id,
        "total-weight" := token-weight,
        "curr-owned-weight":= curr-weight,
        "owners" := owners
      }
      (let* (
         (owner-weight:integer (get-owner-weight account owners) )
         (new-weight:integer (floor (+ owner-weight amount)))
         (new-curr-weight:integer (floor (+ curr-weight amount)))
         (new-owners (if (> new-weight 0)
                        (+ [{'id: (at 'id token),
                             'owner: account,
                             'guard: guard,
                             'weight: new-weight}] (filter (not-owner account) owners) )
                        (filter (not-owner account) owners)
                        )))
         (write ir-tokens (at 'id token)
           { "id" : (at 'id token),
             "total-weight" : token-weight,
             "curr-owned-weight": new-curr-weight,
             "owners" : new-owners
           })
       (emit-event (UPDATE_OWNER (at 'id token) account guard new-weight)))
    ))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce false "Burn prohibited")
  )

  ;; code here taken below straight from fixed-quote policy
  ;; make sure seller is kosher; if not, fails.
  ;; for IR amount = weight always
  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (coin.details recipient))
            (sale-price:decimal (* amount price)) )
      (coin.enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer amount must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (emit-event (QUOTE sale-id (at 'id token) recipient amount price sale-price))
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec }))
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (with-read ir-tokens (at 'id token) {
        "id" := id,
        "total-weight" := token-weight, ;; not used
        "owners" := owners
      }
      (with-read quotes sale-id {
        'id := qtoken,
        'spec := spec:object{quote-spec}
        }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          }
        (with-capability (INTERNAL)
          (let*
            ( (owner-type (not-partial-owner token buyer))
              (owner:list (get-owner seller owners))
            )
            (enforce owner-type "Partial owner is restricted from buying")
            (if (> (length owner) 0)
              (update-owner token seller (at 'guard (at 0 owner)) (* -1.0 amount))
              true
            ))
          (let*
            ( (tokens:list (select ir-tokens (where 'curr-owned-weight (!= 0))))
              (sale-price:decimal (* amount price))
              (total-owned:decimal (fold (+) 0.0 (map (at 'curr-owned-weight ) tokens)))
              (share (* sale-price ROYALTY))
              (balance:decimal (if (> total-owned 0.0) (- sale-price share) sale-price)))
            (coin.enforce-unit sale-price)
            (if (> total-owned 0.0)
              (map (credit-owner buyer share total-owned) (fold (+) [] (map (at 'owners) tokens)))
              true)
            (update-owner token buyer buyer-guard amount)
            (coin.transfer buyer recipient balance)
         )
    )))))

  (defun credit-owner (buyer:string share:decimal total-owned:decimal owner:object{ir-owner})
    (require-capability (INTERNAL))
    (if (!= buyer (at 'owner owner))
      (coin.transfer buyer (at 'owner owner) (floor  (/ (* share (at 'weight owner)) total-owned) (coin.precision)))
      true)
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
      ;; for now, this NFT stays on this chain.
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )
)

(if (read-msg 'upgrade)
  ["upgrade complete"]
  [ (create-table quotes)
    (create-table ir-tokens) ])
