-- https://github.com/sendgrid/sendgrid-oai
module TTHouse.Api.Foreign.SendGrid where

import Prelude

import Data.Function.Uncurried (Fn1, Fn5)
import Foreign (Foreign)
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Effect (Effect)
import Effect.Aff.Compat as AC

foreign import data ApiClient :: Type
foreign import data MailSendApi :: Type

type ApiKey = String

foreign import mkApiClient :: Fn1 ApiKey (Effect ApiClient)

newtype Email = Email { email :: String }
-- https://docs.sendgrid.com/for-developers/sending-email/personalizations
type Personalization = { to :: Email, subject :: String }

type Subject = String 
type Content = String

foreign import mkMailSendApi :: Fn1 ApiClient (Effect MailSendApi)

foreign import send :: Fn5 (NonEmptyArray Personalization) Email Subject Content MailSendApi (AC.EffectFnAff Foreign)