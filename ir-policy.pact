(namespace (read-msg 'ns))

(module policy GOVERNANCE

  @doc "Immutable Record token policy."

  (defcap GOVERNANCE ()
    (enforce-keyset 'ir-admin))

  (implements kip.token-policy-v1_DRAFT3)
  (use kip.token-policy-v1_DRAFT3 [token-info])

  (defschema ir-token
    id:string
    weight:integer
    owner:string
  )

  (deftable ir-tokens:{ir-token})

  (defconst SHARE 0.1
    "Share divided amongst collection owners")

  (defconst QUOTE "quote"
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

  (defcap UPDATE_OWNER (id:string owner:string)
    @event true)

  (defcap INTERNAL () true)

  (defcap CREATE_TOKEN (id:string)
    (enforce-ledger)
    (enforce-keyset 'ir-admin)
  )

  (defcap MINT (id:string)
    (enforce-ledger)
    (enforce-keyset 'ir-admin)
  )

  (defun get-policy:object{ir-token} (token:object{token-info})
    (read ir-tokens (at 'id token))
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (with-capability (CREATE_TOKEN (at 'id token))
      (let* ( (weight (read-integer 'weight))
            )
        (enforce (> weight 0) "Invalid weight")
        (enforce (= (at 'precision token) 1) "Invalid precision")
        (insert ir-tokens (at 'id token)
          { 'weight: weight, 'owner: "", 'id: (at 'id token) })
        true))
  )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (with-capability (MINT (at 'id token))
      (let*
        ( (policy (get-policy token))
          (owner (try "" (read-msg 'owner)))
        )
        (enforce (= (at 'supply token) 0.0) "enforce 1-off supply")
        (enforce (= amount 1.0) "enforce 1-off amount")
        (if (!= owner "")
          (with-capability (INTERNAL) (update-owner token owner))
          true)
      ))
  )

  (defun update-owner (token:object{token-info} owner:string)
    (require-capability (INTERNAL))
    (coin.details owner) ;; enforce coin account
    (update ir-tokens (at 'id token) { 'owner: owner})
    (emit-event (UPDATE_OWNER (at 'id token) owner))
  )


  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (let* ( (spec:object{quote-spec} (read-msg QUOTE))
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
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec }))
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'weight := weight:integer
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          , 'recipient-guard := recipient-guard:guard
          }
          (let*
            ( (sale-price (* amount price))
              (owned (select ir-tokens
                (and? (where 'owner (!= ""))
                      (where 'id (!= (at 'id token))))))
              (total-owned (fold (+) 0.0 (map (at 'weight ) owned)))
              (share (* sale-price SHARE))
              (balance (if (> total-owned 0.0) (- sale-price share) sale-price))
            )
            (with-capability (INTERNAL)
              (map (credit-owner buyer share total-owned) owned)
              (update-owner token buyer))
            (coin.transfer-create buyer recipient recipient-guard balance)))))
  )

  (defun credit-owner (buyer:string share:decimal total-owned:decimal token:object{ir-token})
    (require-capability (INTERNAL))
    (coin.transfer buyer (at 'owner token) (floor (* share (/ (at 'weight token) total-owned)) (coin.precision)))
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      receiver:string
      amount:decimal )
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
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
