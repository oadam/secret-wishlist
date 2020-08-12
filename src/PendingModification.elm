module PendingModification exposing (PendingModification)

import Api exposing (Present)


type PendingModificationState
    = Pending
    | Done
    | Failed



-- before->after (so that we can rollback)


type PendingModification
    = PendingModification ( Present, Present )
