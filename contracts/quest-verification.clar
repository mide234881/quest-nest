;; quest-verification
;; This contract handles verification of completed quests using multiple mechanisms.
;; It enables both automated verification (through oracle integrations) and peer-based verification,
;; allowing users to submit evidence that is validated according to the quest's verification rules.
;; The contract supports various levels of verification from self-reporting to multi-signature verification.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-QUEST-NOT-FOUND (err u1001))
(define-constant ERR-INVALID-VERIFICATION-TYPE (err u1002))
(define-constant ERR-ALREADY-VERIFIED (err u1003))
(define-constant ERR-VERIFICATION-EXPIRED (err u1004))
(define-constant ERR-INSUFFICIENT-VERIFIERS (err u1005))
(define-constant ERR-ALREADY-VERIFIED-BY-USER (err u1006))
(define-constant ERR-NOT-DESIGNATED-VERIFIER (err u1007))
(define-constant ERR-EVIDENCE-REQUIRED (err u1008))
(define-constant ERR-QUEST-NOT-ACTIVE (err u1009))
(define-constant ERR-TOO-MANY-VERIFICATION-ATTEMPTS (err u1010))

;; Verification types
(define-constant VERIFICATION-SELF u1)  ;; Self-reporting, user verifies their own completion
(define-constant VERIFICATION-PEER u2)  ;; Peer verification, requires specified number of peers to verify
(define-constant VERIFICATION-EXPERT u3)  ;; Expert verification, requires specific designated verifiers
(define-constant VERIFICATION-ORACLE u4)  ;; Oracle verification, automated through external data

;; Quest status constants
(define-constant STATUS-PENDING u1)     ;; Verification has been submitted but not confirmed
(define-constant STATUS-VERIFIED u2)    ;; Quest has been verified as completed
(define-constant STATUS-REJECTED u3)    ;; Verification was rejected

;; Data maps

;; Map of all quests and their verification requirements
(define-map quests
  { quest-id: uint }
  {
    creator: principal,
    verification-type: uint,
    required-verifications: uint,
    designated-verifiers: (optional (list 10 principal)),
    oracle-source: (optional (string-utf8 100)),
    verification-deadline: uint,
    is-active: bool
  }
)

;; Map of verification submissions for quests
(define-map verification-submissions
  { quest-id: uint, user: principal }
  {
    status: uint,
    evidence-hash: (optional (buff 32)),
    evidence-url: (optional (string-utf8 200)),
    submission-time: uint,
    verification-count: uint,
    verification-attempts: uint
  }
)

;; Map tracking verifications from specific verifiers
(define-map verifications
  { quest-id: uint, user: principal, verifier: principal }
  {
    verified: bool,
    verification-time: uint,
    comment: (optional (string-utf8 200))
  }
)

;; Private functions

;; Helper function to check if a principal is in a list of principals
(define-private (principal-in-list (user principal) (user-list (list 10 principal)))
  (is-some (index-of user-list user))
)

;; Helper function to check if a quest exists and is active
(define-private (is-quest-active (quest-id uint))
  (match (map-get? quests { quest-id: quest-id })
    quest-data (get is-active quest-data)
    false
  )
)

;; Helper function to check if the sender is authorized to verify a quest
(define-private (is-authorized-verifier (quest-id uint) (user principal))
  (match (map-get? quests { quest-id: quest-id })
    quest-data 
    (let ((verification-type (get verification-type quest-data)))
      (cond
        ;; Self verification - only the user can verify
        (is-eq verification-type VERIFICATION-SELF) (is-eq tx-sender user)
        
        ;; Peer verification - any user can verify
        (is-eq verification-type VERIFICATION-PEER) true
        
        ;; Expert verification - must be in the designated verifiers list
        (is-eq verification-type VERIFICATION-EXPERT)
          (match (get designated-verifiers quest-data)
            verifiers (principal-in-list tx-sender verifiers)
            false
          )
        
        ;; Oracle verification - contract owner only (would connect to oracle)
        (is-eq verification-type VERIFICATION-ORACLE) (is-eq tx-sender (contract-owner))
        
        ;; Default case
        false
      ))
    false
  )
)

;; Helper function to check if a quest has been verified by a specific user
(define-private (has-verified (quest-id uint) (user principal) (verifier principal))
  (match (map-get? verifications { quest-id: quest-id, user: user, verifier: verifier })
    verification-data (get verified verification-data)
    false
  )
)

;; Helper function to update verification status after a new verification is added
(define-private (update-verification-status (quest-id uint) (user principal))
  (match (map-get? verification-submissions { quest-id: quest-id, user: user })
    submission
    (match (map-get? quests { quest-id: quest-id })
      quest-data
      (let 
        (
          (current-count (get verification-count submission))
          (required-count (get required-verifications quest-data))
          (new-count (+ current-count u1))
        )
        (begin
          ;; Update the verification count
          (map-set verification-submissions
            { quest-id: quest-id, user: user }
            (merge submission { verification-count: new-count })
          )
          
          ;; If we've reached the required number of verifications, mark as verified
          (if (>= new-count required-count)
            (map-set verification-submissions
              { quest-id: quest-id, user: user }
              (merge submission { verification-count: new-count, status: STATUS-VERIFIED })
            )
            true
          )
          
          (ok new-count)
        )
      )
      ERR-QUEST-NOT-FOUND
    )
    ERR-QUEST-NOT-FOUND
  )
)

;; Read-only functions

;; Get quest verification requirements
(define-read-only (get-quest-verification-info (quest-id uint))
  (map-get? quests { quest-id: quest-id })
)

;; Get the verification status for a user's quest
(define-read-only (get-verification-status (quest-id uint) (user principal))
  (map-get? verification-submissions { quest-id: quest-id, user: user })
)

;; Check if a user has verified a specific quest for another user
(define-read-only (has-user-verified (quest-id uint) (user principal) (verifier principal))
  (match (map-get? verifications { quest-id: quest-id, user: user, verifier: verifier })
    verification-data (get verified verification-data)
    false
  )
)

;; Public functions

;; Create a new quest with verification requirements
(define-public (create-quest 
  (quest-id uint)
  (verification-type uint)
  (required-verifications uint)
  (designated-verifiers (optional (list 10 principal)))
  (oracle-source (optional (string-utf8 100)))
  (verification-deadline uint)
)
  (begin
    ;; Validation checks
    (asserts! (or (is-eq verification-type VERIFICATION-SELF)
                 (is-eq verification-type VERIFICATION-PEER)
                 (is-eq verification-type VERIFICATION-EXPERT)
                 (is-eq verification-type VERIFICATION-ORACLE))
             ERR-INVALID-VERIFICATION-TYPE)
    
    ;; For expert verification, designated verifiers must be provided
    (asserts! (or (not (is-eq verification-type VERIFICATION-EXPERT))
                 (is-some designated-verifiers))
             ERR-NOT-DESIGNATED-VERIFIER)
    
    ;; For oracle verification, oracle source must be provided
    (asserts! (or (not (is-eq verification-type VERIFICATION-ORACLE))
                 (is-some oracle-source))
             ERR-EVIDENCE-REQUIRED)
    
    ;; Create the quest with verification requirements
    (map-set quests
      { quest-id: quest-id }
      {
        creator: tx-sender,
        verification-type: verification-type,
        required-verifications: required-verifications,
        designated-verifiers: designated-verifiers,
        oracle-source: oracle-source,
        verification-deadline: verification-deadline,
        is-active: true
      }
    )
    
    (ok true)
  )
)

;; Deactivate a quest (only the creator can do this)
(define-public (deactivate-quest (quest-id uint))
  (match (map-get? quests { quest-id: quest-id })
    quest-data
    (begin
      (asserts! (is-eq (get creator quest-data) tx-sender) ERR-NOT-AUTHORIZED)
      (map-set quests
        { quest-id: quest-id }
        (merge quest-data { is-active: false })
      )
      (ok true)
    )
    ERR-QUEST-NOT-FOUND
  )
)

;; Submit a quest for verification
(define-public (submit-for-verification 
  (quest-id uint)
  (evidence-hash (optional (buff 32)))
  (evidence-url (optional (string-utf8 200)))
)
  (begin
    ;; Check if quest exists and is active
    (asserts! (is-quest-active quest-id) ERR-QUEST-NOT-ACTIVE)
    
    ;; Check if verification has already been submitted
    (asserts! (is-none (map-get? verification-submissions { quest-id: quest-id, user: tx-sender })) 
              ERR-ALREADY-VERIFIED)
    
    ;; Check quest verification requirements - evidence is required for non-self verification
    (match (map-get? quests { quest-id: quest-id })
      quest-data
      (begin
        (asserts! (or (is-eq (get verification-type quest-data) VERIFICATION-SELF)
                     (is-some evidence-url)
                     (is-some evidence-hash))
                 ERR-EVIDENCE-REQUIRED)
        
        ;; Self-verification is automatically approved if that's the quest type
        (let ((status (if (is-eq (get verification-type quest-data) VERIFICATION-SELF)
                        STATUS-VERIFIED
                        STATUS-PENDING)))
          
          ;; Create submission record
          (map-set verification-submissions
            { quest-id: quest-id, user: tx-sender }
            {
              status: status,
              evidence-hash: evidence-hash,
              evidence-url: evidence-url,
              submission-time: block-height,
              verification-count: (if (is-eq status STATUS-VERIFIED) u1 u0),
              verification-attempts: u0
            }
          )
          
          ;; If self-verification, also record the verification
          (if (is-eq (get verification-type quest-data) VERIFICATION-SELF)
            (map-set verifications
              { quest-id: quest-id, user: tx-sender, verifier: tx-sender }
              {
                verified: true,
                verification-time: block-height,
                comment: none
              }
            )
            true
          )
          
          (ok status)
        )
      )
      ERR-QUEST-NOT-FOUND
    )
  )
)

;; Verify a quest completion submitted by another user
(define-public (verify-quest-completion
  (quest-id uint)
  (user principal)
  (comment (optional (string-utf8 200)))
)
  (begin
    ;; Check if quest exists and is active
    (asserts! (is-quest-active quest-id) ERR-QUEST-NOT-ACTIVE)
    
    ;; Check if the quest has been submitted for verification
    (match (map-get? verification-submissions { quest-id: quest-id, user: user })
      submission
      (begin
        ;; Check if already verified
        (asserts! (not (is-eq (get status submission) STATUS-VERIFIED)) ERR-ALREADY-VERIFIED)
        
        ;; Check if verification deadline has passed
        (match (map-get? quests { quest-id: quest-id })
          quest-data
          (begin
            (asserts! (<= block-height (get verification-deadline quest-data)) 
                     ERR-VERIFICATION-EXPIRED)
            
            ;; Check if sender is authorized to verify this quest
            (asserts! (is-authorized-verifier quest-id user) ERR-NOT-AUTHORIZED)
            
            ;; Check if this verifier has already verified
            (asserts! (not (has-verified quest-id user tx-sender)) ERR-ALREADY-VERIFIED-BY-USER)
            
            ;; Record the verification
            (map-set verifications
              { quest-id: quest-id, user: user, verifier: tx-sender }
              {
                verified: true,
                verification-time: block-height,
                comment: comment
              }
            )
            
            ;; Update verification status
            (update-verification-status quest-id user)
          )
          ERR-QUEST-NOT-FOUND
        )
      )
      ERR-QUEST-NOT-FOUND
    )
  )
)

;; Reject a quest verification (for expert or oracle verification)
(define-public (reject-verification
  (quest-id uint)
  (user principal)
  (comment (optional (string-utf8 200)))
)
  (begin
    ;; Check if quest exists and is active
    (asserts! (is-quest-active quest-id) ERR-QUEST-NOT-ACTIVE)
    
    ;; Check if the quest has been submitted for verification
    (match (map-get? verification-submissions { quest-id: quest-id, user: user })
      submission
      (begin
        ;; Check if already verified
        (asserts! (not (is-eq (get status submission) STATUS-VERIFIED)) ERR-ALREADY-VERIFIED)
        
        ;; Check if verification attempts limit is reached
        (asserts! (< (get verification-attempts submission) u3) ERR-TOO-MANY-VERIFICATION-ATTEMPTS)
        
        ;; Check if sender is authorized to reject for this quest
        (match (map-get? quests { quest-id: quest-id })
          quest-data
          (begin
            ;; Only designated experts or contract owner (for oracle) can reject
            (asserts!
              (or
                (and (is-eq (get verification-type quest-data) VERIFICATION-EXPERT)
                     (match (get designated-verifiers quest-data)
                       verifiers (principal-in-list tx-sender verifiers)
                       false))
                (and (is-eq (get verification-type quest-data) VERIFICATION-ORACLE)
                     (is-eq tx-sender (contract-owner))))
              ERR-NOT-AUTHORIZED)
            
            ;; Record the rejection
            (map-set verification-submissions
              { quest-id: quest-id, user: user }
              {
                status: STATUS-REJECTED,
                evidence-hash: (get evidence-hash submission),
                evidence-url: (get evidence-url submission),
                submission-time: (get submission-time submission),
                verification-count: u0,
                verification-attempts: (+ (get verification-attempts submission) u1)
              }
            )
            
            ;; Record the rejection from this verifier
            (map-set verifications
              { quest-id: quest-id, user: user, verifier: tx-sender }
              {
                verified: false,
                verification-time: block-height,
                comment: comment
              }
            )
            
            (ok STATUS-REJECTED)
          )
          ERR-QUEST-NOT-FOUND
        )
      )
      ERR-QUEST-NOT-FOUND
    )
  )
)

;; Oracle-based verification (can only be called by contract owner)
(define-public (oracle-verify-quest
  (quest-id uint)
  (user principal)
  (comment (optional (string-utf8 200)))
)
  (begin
    ;; Check if sender is contract owner
    (asserts! (is-eq tx-sender (contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Check if quest exists and is active
    (asserts! (is-quest-active quest-id) ERR-QUEST-NOT-ACTIVE)
    
    ;; Check if the quest has been submitted for verification
    (match (map-get? verification-submissions { quest-id: quest-id, user: user })
      submission
      (begin
        ;; Check if already verified
        (asserts! (not (is-eq (get status submission) STATUS-VERIFIED)) ERR-ALREADY-VERIFIED)
        
        ;; Check if quest is oracle type
        (match (map-get? quests { quest-id: quest-id })
          quest-data
          (begin
            (asserts! (is-eq (get verification-type quest-data) VERIFICATION-ORACLE) 
                     ERR-INVALID-VERIFICATION-TYPE)
            
            ;; Mark as verified by oracle
            (map-set verification-submissions
              { quest-id: quest-id, user: user }
              (merge submission { status: STATUS-VERIFIED, verification-count: u1 })
            )
            
            ;; Record oracle verification
            (map-set verifications
              { quest-id: quest-id, user: user, verifier: tx-sender }
              {
                verified: true,
                verification-time: block-height,
                comment: comment
              }
            )
            
            (ok STATUS-VERIFIED)
          )
          ERR-QUEST-NOT-FOUND
        )
      )
      ERR-QUEST-NOT-FOUND
    )
  )
)