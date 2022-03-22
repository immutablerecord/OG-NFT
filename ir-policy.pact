(namespace (read-msg 'ns))

(module policy GOVERNANCE

  @doc "Immutable Record token policy."

  (defcap GOVERNANCE ()
    (enforce-keyset 'ir-admin))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])

  (defschema ir-token
    id:string
    weight:integer
  )

  (defschema ir-owner
    id:string
    weight:decimal
    owner:string
    guard:guard
  )

  (deftable ir-tokens:{ir-token})
  (deftable ir-owners:{ir-owner})

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

  (defcap UPDATE_OWNER (id:string owner:string guard:guard amount:decimal)
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

  (defun get-owner:object{ir-owner} (token:object{token-info} account:string)
    (read ir-owners (key (at 'id token) account))
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
          {'weight: weight, 'id: id})
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
        (enforce (= amount (+ 0.0 (at 'weight policy))) "enforce 1-off amount") ;; look for a better way to convert weight into decimal
        (with-capability (INTERNAL) (update-owner token account guard 0.0))
      ))
  )

  (defun update-owner (token:object{token-info} account:string guard:guard amount:decimal)
    (require-capability (INTERNAL))
    (if (!= amount 0.0)
      (let ((coin-guard:guard (at 'guard (coin.details account))))
        (enforce (= coin-guard guard) "Account guard does not match")
      )
      true)
    (with-default-read ir-owners (key (at 'id token) account)
      { "weight" : -1.0 }
      { "weight" := weight }
      (let* ((is-new
             (if (= weight -1.0)
                 true
               false))
            (new-weight
             (if is-new amount (+ weight amount))))
      (enforce (>= new-weight 0.0) "Amount not positive")
      (write ir-owners (key (at 'id token) account)
        { "id" : (at 'id token),
          "weight" : new-weight,
          "owner" : account,
          "guard" : guard
        })
    ;; IR will make sure that buyers have k: accounts
    (emit-event (UPDATE_OWNER (at 'id token) account guard new-weight)))) ;; emit guard?
  )

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
    (bind (get-owner token seller) {'guard := seller-guard, 'weight := seller-weight}
      (with-read quotes sale-id { 'id := qtoken, 'spec := spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          , 'recipient-guard := recipient-guard:guard
          }
          (with-capability (INTERNAL)
            (if (> seller-weight 0.0)
              (update-owner token seller seller-guard (* -1.0 amount))
              true)
            (let*
              ( (owned (select ir-owners (where 'weight (!= 0.0))))
                (sale-price (* amount price))
                (total-owned (fold (+) 0.0 (map (at 'weight ) owned)))
                (share (* sale-price ROYALTY))
                (balance (if (> total-owned 0.0) (- sale-price share) sale-price)))
              (coin.enforce-unit sale-price)
              (if (> total-owned 0.0)
                (map (credit-owner buyer share total-owned) owned)
                true)
              (update-owner token buyer buyer-guard amount)
              (coin.transfer-create buyer recipient recipient-guard balance))))))
  )

  (defun credit-owner (buyer:string share:decimal total-owned:decimal owner:object{ir-owner})
    (require-capability (INTERNAL))
    (coin.transfer buyer (at 'owner owner) (floor (* share (/ (at 'weight owner) total-owned)) (coin.precision)))
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
    (create-table ir-tokens)
    (create-table ir-owners) ])
