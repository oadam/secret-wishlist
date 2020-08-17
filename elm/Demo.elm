module Demo exposing (presents, users)

import Present exposing (Present)


users : List String
users =
    [ "papa", "maman", "tonton" ]


presents : List Present
presents =
    [ { id = 1
      , user = "papa"
      , subject = "bd fantaisy"
      , content = "une bd comme on les aimes"
      }
    ]
