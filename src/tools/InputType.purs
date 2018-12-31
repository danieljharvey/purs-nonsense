module InputType where

import Prelude (Unit)

import Effect (Effect)
import Interaction (InteractionState)
import Validators

type ReturnType e a =
  { interactionState :: InteractionState
  , inputState :: Validator e a
  }

type FormInput e a =
  { initial :: a
  , validate :: Validator e a
  , update :: (ReturnType e a -> Effect Unit)
}
