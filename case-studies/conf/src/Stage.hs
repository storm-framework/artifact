module Stage where 

{-@ measure currentStage :: String @-}

{-@ assume currentStage :: {v: String | currentStage == v} @-}
currentStage :: String
currentStage = "submit"
