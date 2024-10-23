-- VERSION: 0.1.0.0
module TestSpec (spec) where

import Engine ((=~))
import qualified Text.Regex.TDFA as TDFA
import Test.Hspec
import Control.Exception (tryJust, SomeException)
import Control.Monad (guard)
import Data.List (isInfixOf)

runTDFATest :: IO a -> Expectation
runTDFATest action = do
  result <- tryJust (guard . isTDFAException) action
  case result of
    Left _ -> pendingWith $ "TDFA threw an exception: "
    Right _ -> return ()

isTDFAException :: SomeException -> Bool
isTDFAException e = "TDFA" `isInfixOf` show e

spec :: Spec
spec =
    describe "Full matching" $ do
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does match string" $ runTDFATest $ do
        ("Transform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Transf_rm._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transf_rm._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Transf_rm._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transf_rm._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Transf_rm._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transf_rm._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("TransformQ_handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("TransformQ_handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("TransformQ_handle" =~ "Transform\\._handle" :: String) `shouldBe` ("TransformQ_handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("TransformQ_handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("TransformQ_handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Transf]rm._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transf]rm._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Transf]rm._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transf]rm._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Transf]rm._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transf]rm._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Tr6nsform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Tr6nsform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Tr6nsform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Tr6nsform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Tr6nsform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Tr6nsform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("T<ansform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("T<ansform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("T<ansform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("T<ansform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("T<ansform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("T<ansform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Transform._htndle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._htndle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Transform._htndle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._htndle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Transform._htndle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._htndle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Tr~nsform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Tr~nsform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Tr~nsform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Tr~nsform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Tr~nsform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Tr~nsform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Transform._handZe" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform._handZe" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Transform._handZe" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform._handZe" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Transform._handZe" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform._handZe" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Transform.Nhandle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Transform.Nhandle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Transform.Nhandle" =~ "Transform\\._handle" :: String) `shouldBe` ("Transform.Nhandle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Transform.Nhandle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Transform.Nhandle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Traosform._handle" =~ "Transform\\._handle" :: Bool) `shouldBe` ("Traosform._handle" TDFA.=~ "Transform\\._handle" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Traosform._handle" =~ "Transform\\._handle" :: String) `shouldBe` ("Traosform._handle" TDFA.=~ "Transform\\._handle" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Traosform._handle" =~ "Transform\\._handle" :: (String, String, String)) `shouldBe` ("Traosform._handle" TDFA.=~ "Transform\\._handle" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does match string" $ runTDFATest $ do
        ("_mmaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalbndar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalbndar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalbndar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalbndar" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalbndar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalbndar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalendSr" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalendSr" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalendSr" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalendSr" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalendSr" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalendSr" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalen:ar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalen:ar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalen:ar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalen:ar" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalen:ar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalen:ar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mmaCUlendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCUlendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCUlendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCUlendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCUlendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCUlendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mDaCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mDaCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mDaCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mDaCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mDaCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mDaCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalenda'" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalenda'" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalenda'" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalenda'" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalenda'" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalenda'" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalen9ar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalen9ar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalen9ar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalen9ar" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalen9ar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalen9ar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mm8Calendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mm8Calendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mm8Calendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mm8Calendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mm8Calendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mm8Calendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_:maCalendar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_:maCalendar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_:maCalendar" =~ "_mmaCalendar" :: String) `shouldBe` ("_:maCalendar" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_:maCalendar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_:maCalendar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalfndar" =~ "_mmaCalendar" :: Bool) `shouldBe` ("_mmaCalfndar" TDFA.=~ "_mmaCalendar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalfndar" =~ "_mmaCalendar" :: String) `shouldBe` ("_mmaCalfndar" TDFA.=~ "_mmaCalendar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_mmaCalfndar" =~ "_mmaCalendar" :: (String, String, String)) `shouldBe` ("_mmaCalfndar" TDFA.=~ "_mmaCalendar" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID==mG9vXXK,Number=i.o@aW#D,Type=+j6|,Description=\"8\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool) `shouldBe` ("##FORMAT=<ID==mG9vXXK,Number=i.o@aW#D,Type=+j6|,Description=\"8\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID==mG9vXXK,Number=i.o@aW#D,Type=+j6|,Description=\"8\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String) `shouldBe` ("##FORMAT=<ID==mG9vXXK,Number=i.o@aW#D,Type=+j6|,Description=\"8\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String)
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID==mG9vXXK,Number=i.o@aW#D,Type=+j6|,Description=\"8\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String)) `shouldBe` ("##FORMAT=<ID==mG9vXXK,Number=i.o@aW#D,Type=+j6|,Description=\"8\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=|2,Number=fP@,Type=F64sw,Description=\"X1,ruHc\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool) `shouldBe` ("##FORMAT=<ID=|2,Number=fP@,Type=F64sw,Description=\"X1,ruHc\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=|2,Number=fP@,Type=F64sw,Description=\"X1,ruHc\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String) `shouldBe` ("##FORMAT=<ID=|2,Number=fP@,Type=F64sw,Description=\"X1,ruHc\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String)
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=|2,Number=fP@,Type=F64sw,Description=\"X1,ruHc\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String)) `shouldBe` ("##FORMAT=<ID=|2,Number=fP@,Type=F64sw,Description=\"X1,ruHc\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=&I0iD1(,Number=pG/;`],Type=dT8Y2,Description=\"\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool) `shouldBe` ("##FORMAT=<ID=&I0iD1(,Number=pG/;`],Type=dT8Y2,Description=\"\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=&I0iD1(,Number=pG/;`],Type=dT8Y2,Description=\"\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String) `shouldBe` ("##FORMAT=<ID=&I0iD1(,Number=pG/;`],Type=dT8Y2,Description=\"\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String)
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=&I0iD1(,Number=pG/;`],Type=dT8Y2,Description=\"\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String)) `shouldBe` ("##FORMAT=<ID=&I0iD1(,Number=pG/;`],Type=dT8Y2,Description=\"\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=3g4@,Number=etWmb^YSM:,Type=;PAAw,Description=\"y_>MEv!\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool) `shouldBe` ("##FORMAT=<ID=3g4@,Number=etWmb^YSM:,Type=;PAAw,Description=\"y_>MEv!\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=3g4@,Number=etWmb^YSM:,Type=;PAAw,Description=\"y_>MEv!\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String) `shouldBe` ("##FORMAT=<ID=3g4@,Number=etWmb^YSM:,Type=;PAAw,Description=\"y_>MEv!\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String)
      it "does match string" $ runTDFATest $ do
        ("##FORMAT=<ID=3g4@,Number=etWmb^YSM:,Type=;PAAw,Description=\"y_>MEv!\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String)) `shouldBe` ("##FORMAT=<ID=3g4@,Number=etWmb^YSM:,Type=;PAAw,Description=\"y_>MEv!\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("##FORMAT=<ID=%9^Q.]f,Number=|Y'EpY8,|ype=o0,Description=\"uH&wN\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool) `shouldBe` ("##FORMAT=<ID=%9^Q.]f,Number=|Y'EpY8,|ype=o0,Description=\"uH&wN\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("##FORMAT=<ID=%9^Q.]f,Number=|Y'EpY8,|ype=o0,Description=\"uH&wN\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String) `shouldBe` ("##FORMAT=<ID=%9^Q.]f,Number=|Y'EpY8,|ype=o0,Description=\"uH&wN\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String)
      it "does not match string" $ runTDFATest $ do
        ("##FORMAT=<ID=%9^Q.]f,Number=|Y'EpY8,|ype=o0,Description=\"uH&wN\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String)) `shouldBe` ("##FORMAT=<ID=%9^Q.]f,Number=|Y'EpY8,|ype=o0,Description=\"uH&wN\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("##FORMAT=<ID=&V,Number='',TyOe=YB@t,Description=\"+E$B?/\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool) `shouldBe` ("##FORMAT=<ID=&V,Number='',TyOe=YB@t,Description=\"+E$B?/\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("##FORMAT=<ID=&V,Number='',TyOe=YB@t,Description=\"+E$B?/\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String) `shouldBe` ("##FORMAT=<ID=&V,Number='',TyOe=YB@t,Description=\"+E$B?/\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String)
      it "does not match string" $ runTDFATest $ do
        ("##FORMAT=<ID=&V,Number='',TyOe=YB@t,Description=\"+E$B?/\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String)) `shouldBe` ("##FORMAT=<ID=&V,Number='',TyOe=YB@t,Description=\"+E$B?/\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("##FOhMAT=<ID=9LP}TBc,Number=%,Type=7Ba4acS],Description=\"hug!:U+=&C\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool) `shouldBe` ("##FOhMAT=<ID=9LP}TBc,Number=%,Type=7Ba4acS],Description=\"hug!:U+=&C\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("##FOhMAT=<ID=9LP}TBc,Number=%,Type=7Ba4acS],Description=\"hug!:U+=&C\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String) `shouldBe` ("##FOhMAT=<ID=9LP}TBc,Number=%,Type=7Ba4acS],Description=\"hug!:U+=&C\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: String)
      it "does not match string" $ runTDFATest $ do
        ("##FOhMAT=<ID=9LP}TBc,Number=%,Type=7Ba4acS],Description=\"hug!:U+=&C\">" =~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String)) `shouldBe` ("##FOhMAT=<ID=9LP}TBc,Number=%,Type=7Ba4acS],Description=\"hug!:U+=&C\">" TDFA.=~ "##FORMAT=<ID=([^,]+),Number=([^,]+),Type=([^,]+),Description=\"([^\"]*)\">" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)R" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)R" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)R" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)R" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)R" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line: 6)R" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated rtsource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated rtsource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated rtsource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated rtsource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated rtsource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated rtsource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with nel values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with nel values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with nel values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with nel values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with nel values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with nel values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an alrevdy evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an alrevdy evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an alrevdy evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an alrevdy evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an alrevdy evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an alrevdy evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at vline: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at vline: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at vline: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at vline: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at vline: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at vline: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Attempt to ovOrride an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to ovOrride an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to ovOrride an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to ovOrride an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to ovOrride an already evaluated resource, defined at (line: 4), with new values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to ovOrride an already evaluated resource, defined at (line: 4), with new values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with \\ew values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with \\ew values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with \\ew values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with \\ew values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with \\ew values (line: 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with \\ew values (line: 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line4 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line4 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line4 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line4 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line4 6)" =~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String)) `shouldBe` ("Attempt to override an already evaluated resource, defined at (line: 4), with new values (line4 6)" TDFA.=~ "Attempt to override an already evaluated resource, defined at \\(line: 4\\), with new values \\(line: 6\\)" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("Category:  " =~ "Category:\\s*" :: Bool) `shouldBe` ("Category:  " TDFA.=~ "Category:[[:space:]]*" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Category:  " =~ "Category:\\s*" :: String) `shouldBe` ("Category:  " TDFA.=~ "Category:[[:space:]]*" :: String)
      it "does match string" $ runTDFATest $ do
        ("Category:  " =~ "Category:\\s*" :: (String, String, String)) `shouldBe` ("Category:  " TDFA.=~ "Category:[[:space:]]*" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: Bool) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: String) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: Bool) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: String) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: Bool) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: String) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: Bool) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: String) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: Bool) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: String) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: Bool) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: String) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: Bool) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: String) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: Bool) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: String) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: Bool) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: String) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("mswin" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mswin" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: Bool) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: String) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: String)
      it "does match string" $ runTDFATest $ do
        ("Windows" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("Windows" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mswiR" =~ "Windows|mswin" :: Bool) `shouldBe` ("mswiR" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mswiR" =~ "Windows|mswin" :: String) `shouldBe` ("mswiR" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mswiR" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mswiR" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mrwin" =~ "Windows|mswin" :: Bool) `shouldBe` ("mrwin" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mrwin" =~ "Windows|mswin" :: String) `shouldBe` ("mrwin" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mrwin" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mrwin" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("WinXows" =~ "Windows|mswin" :: Bool) `shouldBe` ("WinXows" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("WinXows" =~ "Windows|mswin" :: String) `shouldBe` ("WinXows" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("WinXows" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("WinXows" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("WRndows" =~ "Windows|mswin" :: Bool) `shouldBe` ("WRndows" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("WRndows" =~ "Windows|mswin" :: String) `shouldBe` ("WRndows" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("WRndows" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("WRndows" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Windows@" =~ "Windows|mswin" :: Bool) `shouldBe` ("Windows@" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Windows@" =~ "Windows|mswin" :: String) `shouldBe` ("Windows@" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Windows@" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("Windows@" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mswi3" =~ "Windows|mswin" :: Bool) `shouldBe` ("mswi3" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mswi3" =~ "Windows|mswin" :: String) `shouldBe` ("mswi3" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mswi3" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mswi3" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("(swin" =~ "Windows|mswin" :: Bool) `shouldBe` ("(swin" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("(swin" =~ "Windows|mswin" :: String) `shouldBe` ("(swin" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("(swin" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("(swin" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Wind%ws" =~ "Windows|mswin" :: Bool) `shouldBe` ("Wind%ws" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Wind%ws" =~ "Windows|mswin" :: String) `shouldBe` ("Wind%ws" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Wind%ws" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("Wind%ws" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mswiv" =~ "Windows|mswin" :: Bool) `shouldBe` ("mswiv" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mswiv" =~ "Windows|mswin" :: String) `shouldBe` ("mswiv" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mswiv" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("mswiv" TDFA.=~ "Windows|mswin" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("WindowE" =~ "Windows|mswin" :: Bool) `shouldBe` ("WindowE" TDFA.=~ "Windows|mswin" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("WindowE" =~ "Windows|mswin" :: String) `shouldBe` ("WindowE" TDFA.=~ "Windows|mswin" :: String)
      it "does not match string" $ runTDFATest $ do
        ("WindowE" =~ "Windows|mswin" :: (String, String, String)) `shouldBe` ("WindowE" TDFA.=~ "Windows|mswin" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does match string" $ runTDFATest $ do
        ("PatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("PatchPe;l.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPe;l.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("PatchPe;l.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPe;l.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("PatchPe;l.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPe;l.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("PatchPeWl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPeWl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("PatchPeWl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPeWl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("PatchPeWl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPeWl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("XatchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("XatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("XatchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("XatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("XatchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("XatchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Patchperl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("Patchperl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Patchperl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("Patchperl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Patchperl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("Patchperl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("P'tchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("P'tchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("P'tchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("P'tchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("P'tchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("P'tchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("PaUchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PaUchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("PaUchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PaUchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("PaUchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PaUchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("PatchPeWl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PatchPeWl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("PatchPeWl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PatchPeWl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("PatchPeWl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PatchPeWl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("PCtchPerl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("PCtchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("PCtchPerl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("PCtchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("PCtchPerl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("PCtchPerl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Patchperl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("Patchperl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Patchperl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("Patchperl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Patchperl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("Patchperl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Patch.erl.pm" =~ "PatchPerl\\.pm" :: Bool) `shouldBe` ("Patch.erl.pm" TDFA.=~ "PatchPerl\\.pm" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Patch.erl.pm" =~ "PatchPerl\\.pm" :: String) `shouldBe` ("Patch.erl.pm" TDFA.=~ "PatchPerl\\.pm" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Patch.erl.pm" =~ "PatchPerl\\.pm" :: (String, String, String)) `shouldBe` ("Patch.erl.pm" TDFA.=~ "PatchPerl\\.pm" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: Bool) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: String) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: Bool) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: String) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: Bool) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: String) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: Bool) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: String) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: Bool) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: String) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: Bool) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: String) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: Bool) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: String) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: Bool) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: String) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ois" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ois" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: Bool) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: String) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: Bool) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: String) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ous" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ous" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("oisn" =~ "o[iu]s$" :: Bool) `shouldBe` ("oisn" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("oisn" =~ "o[iu]s$" :: String) `shouldBe` ("oisn" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("oisn" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("oisn" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("o s" =~ "o[iu]s$" :: Bool) `shouldBe` ("o s" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("o s" =~ "o[iu]s$" :: String) `shouldBe` ("o s" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("o s" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("o s" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ons" =~ "o[iu]s$" :: Bool) `shouldBe` ("ons" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ons" =~ "o[iu]s$" :: String) `shouldBe` ("ons" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ons" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ons" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ois " =~ "o[iu]s$" :: Bool) `shouldBe` ("ois " TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ois " =~ "o[iu]s$" :: String) `shouldBe` ("ois " TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ois " =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ois " TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Nis" =~ "o[iu]s$" :: Bool) `shouldBe` ("Nis" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Nis" =~ "o[iu]s$" :: String) `shouldBe` ("Nis" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Nis" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("Nis" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ousE" =~ "o[iu]s$" :: Bool) `shouldBe` ("ousE" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ousE" =~ "o[iu]s$" :: String) `shouldBe` ("ousE" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ousE" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ousE" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("oi(" =~ "o[iu]s$" :: Bool) `shouldBe` ("oi(" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("oi(" =~ "o[iu]s$" :: String) `shouldBe` ("oi(" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("oi(" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("oi(" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ouS" =~ "o[iu]s$" :: Bool) `shouldBe` ("ouS" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ouS" =~ "o[iu]s$" :: String) `shouldBe` ("ouS" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ouS" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("ouS" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("oiY" =~ "o[iu]s$" :: Bool) `shouldBe` ("oiY" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("oiY" =~ "o[iu]s$" :: String) `shouldBe` ("oiY" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("oiY" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("oiY" TDFA.=~ "o[iu]s$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("o s" =~ "o[iu]s$" :: Bool) `shouldBe` ("o s" TDFA.=~ "o[iu]s$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("o s" =~ "o[iu]s$" :: String) `shouldBe` ("o s" TDFA.=~ "o[iu]s$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("o s" =~ "o[iu]s$" :: (String, String, String)) `shouldBe` ("o s" TDFA.=~ "o[iu]s$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: Bool) `shouldBe` ("end of life" TDFA.=~ "end of life" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: String) `shouldBe` ("end of life" TDFA.=~ "end of life" :: String)
      it "does match string" $ runTDFATest $ do
        ("end of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("end of l@fe" =~ "end of life" :: Bool) `shouldBe` ("end of l@fe" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("end of l@fe" =~ "end of life" :: String) `shouldBe` ("end of l@fe" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("end of l@fe" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of l@fe" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("end of lifeS" =~ "end of life" :: Bool) `shouldBe` ("end of lifeS" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("end of lifeS" =~ "end of life" :: String) `shouldBe` ("end of lifeS" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("end of lifeS" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of lifeS" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("end of l:fe" =~ "end of life" :: Bool) `shouldBe` ("end of l:fe" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("end of l:fe" =~ "end of life" :: String) `shouldBe` ("end of l:fe" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("end of l:fe" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of l:fe" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("enduof life" =~ "end of life" :: Bool) `shouldBe` ("enduof life" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("enduof life" =~ "end of life" :: String) `shouldBe` ("enduof life" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("enduof life" =~ "end of life" :: (String, String, String)) `shouldBe` ("enduof life" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("end of lifeV" =~ "end of life" :: Bool) `shouldBe` ("end of lifeV" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("end of lifeV" =~ "end of life" :: String) `shouldBe` ("end of lifeV" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("end of lifeV" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of lifeV" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("end of gife" =~ "end of life" :: Bool) `shouldBe` ("end of gife" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("end of gife" =~ "end of life" :: String) `shouldBe` ("end of gife" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("end of gife" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of gife" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("end of lif4" =~ "end of life" :: Bool) `shouldBe` ("end of lif4" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("end of lif4" =~ "end of life" :: String) `shouldBe` ("end of lif4" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("end of lif4" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of lif4" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("end of :ife" =~ "end of life" :: Bool) `shouldBe` ("end of :ife" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("end of :ife" =~ "end of life" :: String) `shouldBe` ("end of :ife" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("end of :ife" =~ "end of life" :: (String, String, String)) `shouldBe` ("end of :ife" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("en\" of life" =~ "end of life" :: Bool) `shouldBe` ("en\" of life" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("en\" of life" =~ "end of life" :: String) `shouldBe` ("en\" of life" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("en\" of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("en\" of life" TDFA.=~ "end of life" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("gnd of life" =~ "end of life" :: Bool) `shouldBe` ("gnd of life" TDFA.=~ "end of life" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("gnd of life" =~ "end of life" :: String) `shouldBe` ("gnd of life" TDFA.=~ "end of life" :: String)
      it "does not match string" $ runTDFATest $ do
        ("gnd of life" =~ "end of life" :: (String, String, String)) `shouldBe` ("gnd of life" TDFA.=~ "end of life" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does match string" $ runTDFATest $ do
        ("GRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEr" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEr" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEr" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEr" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEr" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEr" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEm" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEm" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEm" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEm" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEm" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEm" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEj" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEj" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEj" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEj" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEj" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEj" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("mRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("mRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("mRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("GRID_jPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_jPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("GRID_jPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_jPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("GRID_jPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_jPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("hRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("hRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("hRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("hRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("hRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("hRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("TRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("TRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("TRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("TRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("TRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("TRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEMJ" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEMJ" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEMJ" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEMJ" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEMJ" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEMJ" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEM5" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("GRID_PPEM5" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEM5" =~ "^GRID_PPEM" :: String) `shouldBe` ("GRID_PPEM5" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("GRID_PPEM5" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("GRID_PPEM5" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cRID_PPEM" =~ "^GRID_PPEM" :: Bool) `shouldBe` ("cRID_PPEM" TDFA.=~ "^GRID_PPEM" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cRID_PPEM" =~ "^GRID_PPEM" :: String) `shouldBe` ("cRID_PPEM" TDFA.=~ "^GRID_PPEM" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cRID_PPEM" =~ "^GRID_PPEM" :: (String, String, String)) `shouldBe` ("cRID_PPEM" TDFA.=~ "^GRID_PPEM" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("I_z:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("I_z:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("I_z:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("I_z:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("I_z:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("I_z:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("UOww087P:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("UOww087P:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("UOww087P:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("UOww087P:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("UOww087P:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("UOww087P:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("45QM54WJ3:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("45QM54WJ3:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("45QM54WJ3:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("45QM54WJ3:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("45QM54WJ3:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("45QM54WJ3:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ldU7:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("ldU7:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ldU7:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("ldU7:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ldU7:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("ldU7:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("KUC:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("KUC:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("KUC:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("KUC:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("KUC:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("KUC:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("9NT:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("9NT:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("9NT:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("9NT:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does match string" $ runTDFATest $ do
        ("9NT:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("9NT:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tape-record-ad\"in" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("tape-record-ad\"in" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tape-record-ad\"in" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("tape-record-ad\"in" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tape-record-ad\"in" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("tape-record-ad\"in" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("LJp&lNlmm:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("LJp&lNlmm:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("LJp&lNlmm:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("LJp&lNlmm:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("LJp&lNlmm:tape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("LJp&lNlmm:tape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("pk:taQe-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("pk:taQe-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("pk:taQe-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("pk:taQe-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("pk:taQe-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("pk:taQe-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("WC:tape-reyord-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("WC:tape-reyord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("WC:tape-reyord-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("WC:tape-reyord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("WC:tape-reyord-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("WC:tape-reyord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tape-record-a!min" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("tape-record-a!min" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tape-record-a!min" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("tape-record-a!min" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tape-record-a!min" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("tape-record-a!min" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("oo3Q:tape-record-adRin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("oo3Q:tape-record-adRin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("oo3Q:tape-record-adRin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("oo3Q:tape-record-adRin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("oo3Q:tape-record-adRin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("oo3Q:tape-record-adRin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("(ape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("(ape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("(ape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("(ape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("(ape-record-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("(ape-record-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tape-re#ord-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("tape-re#ord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tape-re#ord-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("tape-re#ord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tape-re#ord-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("tape-re#ord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tape-{ecord-admin" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("tape-{ecord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tape-{ecord-admin" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("tape-{ecord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tape-{ecord-admin" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("tape-{ecord-admin" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("OmiljlBd:tape-record-admiH" =~ "^(\\w+:)?tape-record-admin$" :: Bool) `shouldBe` ("OmiljlBd:tape-record-admiH" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("OmiljlBd:tape-record-admiH" =~ "^(\\w+:)?tape-record-admin$" :: String) `shouldBe` ("OmiljlBd:tape-record-admiH" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("OmiljlBd:tape-record-admiH" =~ "^(\\w+:)?tape-record-admin$" :: (String, String, String)) `shouldBe` ("OmiljlBd:tape-record-admiH" TDFA.=~ "^([[:word:]]+:)?tape-record-admin$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" /K" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" /K" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" /K" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" /K" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summary\" /K" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summary\" /K" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<metaPname=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<metaPname=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<metaPname=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<metaPname=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<metaPname=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<metaPname=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<m;ta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<m;ta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<m;ta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<m;ta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<m;ta name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<m;ta name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" @ontent=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" @ontent=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" @ontent=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" @ontent=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" @ontent=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" @ontent=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=nsummary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=nsummary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=nsummary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=nsummary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=nsummary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=nsummary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"Zwitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"Zwitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"Zwitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"Zwitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"Zwitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"Zwitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summ)ry\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" content=\"summ)ry\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summ)ry\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" content=\"summ)ry\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" content=\"summ)ry\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" content=\"summ)ry\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" conten==\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twitter:card\" conten==\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" conten==\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twitter:card\" conten==\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twitter:card\" conten==\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twitter:card\" conten==\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twit2er:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta name=\"twit2er:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twit2er:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta name=\"twit2er:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<meta name=\"twit2er:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta name=\"twit2er:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<meta7name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool) `shouldBe` ("<meta7name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<meta7name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String) `shouldBe` ("<meta7name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<meta7name=\"twitter:card\" content=\"summary\" />" =~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String)) `shouldBe` ("<meta7name=\"twitter:card\" content=\"summary\" />" TDFA.=~ "<meta name=\"twitter:card\" content=\"summary\" \\/>" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does match string" $ runTDFATest $ do
        ("use_target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use_target_rub\\" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_rub\\" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use_target_rub\\" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_rub\\" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use_target_rub\\" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_rub\\" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use}target_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use}target_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use}target_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use}target_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use}target_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use}target_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use_tarVet_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_tarVet_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use_tarVet_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_tarVet_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use_tarVet_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_tarVet_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use_trget_ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_trget_ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use_trget_ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_trget_ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use_trget_ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_trget_ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use_target&ruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target&ruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use_target&ruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_target&ruby" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use_target&ruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target&ruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use_targetMruby" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_targetMruby" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use_targetMruby" =~ "use_target_ruby" :: String) `shouldBe` ("use_targetMruby" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use_targetMruby" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_targetMruby" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use_target_ruy" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_ruy" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use_target_ruy" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_ruy" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use_target_ruy" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_ruy" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use_target_r<by" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_r<by" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use_target_r<by" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_r<by" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use_target_r<by" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_r<by" TDFA.=~ "use_target_ruby" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("use_target_rub8" =~ "use_target_ruby" :: Bool) `shouldBe` ("use_target_rub8" TDFA.=~ "use_target_ruby" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("use_target_rub8" =~ "use_target_ruby" :: String) `shouldBe` ("use_target_rub8" TDFA.=~ "use_target_ruby" :: String)
      it "does not match string" $ runTDFATest $ do
        ("use_target_rub8" =~ "use_target_ruby" :: (String, String, String)) `shouldBe` ("use_target_rub8" TDFA.=~ "use_target_ruby" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("\\hackscore {eKk}" =~ "\\\\hackscore\\s*{[^}]*}" :: Bool) `shouldBe` ("\\hackscore {eKk}" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\\hackscore {eKk}" =~ "\\\\hackscore\\s*{[^}]*}" :: String) `shouldBe` ("\\hackscore {eKk}" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: String)
      it "does match string" $ runTDFATest $ do
        ("\\hackscore {eKk}" =~ "\\\\hackscore\\s*{[^}]*}" :: (String, String, String)) `shouldBe` ("\\hackscore {eKk}" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("\\hackscore{]iM}" =~ "\\\\hackscore\\s*{[^}]*}" :: Bool) `shouldBe` ("\\hackscore{]iM}" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\\hackscore{]iM}" =~ "\\\\hackscore\\s*{[^}]*}" :: String) `shouldBe` ("\\hackscore{]iM}" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: String)
      it "does match string" $ runTDFATest $ do
        ("\\hackscore{]iM}" =~ "\\\\hackscore\\s*{[^}]*}" :: (String, String, String)) `shouldBe` ("\\hackscore{]iM}" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("\\hackscore{}9" =~ "\\\\hackscore\\s*{[^}]*}" :: Bool) `shouldBe` ("\\hackscore{}9" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("\\hackscore{}9" =~ "\\\\hackscore\\s*{[^}]*}" :: String) `shouldBe` ("\\hackscore{}9" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: String)
      it "does not match string" $ runTDFATest $ do
        ("\\hackscore{}9" =~ "\\\\hackscore\\s*{[^}]*}" :: (String, String, String)) `shouldBe` ("\\hackscore{}9" TDFA.=~ "\\\\hackscore[[:space:]]*{[^}]*}" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("dotted" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("dotted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("dotted" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("dotted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("dotted" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("dotted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("solid" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("solid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("solid" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("solid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("solid" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("solid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("dotted" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("dotted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("dotted" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("dotted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("dotted" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("dotted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("double" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("double" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("dashed" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("dashed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("solid" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("solid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("solid" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("solid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does match string" $ runTDFATest $ do
        ("solid" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("solid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("dasheS" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("dasheS" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("dasheS" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("dasheS" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("dasheS" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("dasheS" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("soJid" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("soJid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("soJid" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("soJid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("soJid" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("soJid" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("doubleF" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("doubleF" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("doubleF" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("doubleF" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("doubleF" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("doubleF" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("dashedJ" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("dashedJ" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("dashedJ" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("dashedJ" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("dashedJ" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("dashedJ" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("das.ed" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("das.ed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("das.ed" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("das.ed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("das.ed" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("das.ed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("da?hed" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("da?hed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("da?hed" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("da?hed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("da?hed" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("da?hed" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("doubl!" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("doubl!" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("doubl!" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("doubl!" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("doubl!" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("doubl!" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("domted" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("domted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("domted" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("domted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("domted" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("domted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("}otted" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("}otted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("}otted" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("}otted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("}otted" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("}otted" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("dash*d" =~ "^(solid|double|dotted|dashed)$" :: Bool) `shouldBe` ("dash*d" TDFA.=~ "^(solid|double|dotted|dashed)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("dash*d" =~ "^(solid|double|dotted|dashed)$" :: String) `shouldBe` ("dash*d" TDFA.=~ "^(solid|double|dotted|dashed)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("dash*d" =~ "^(solid|double|dotted|dashed)$" :: (String, String, String)) `shouldBe` ("dash*d" TDFA.=~ "^(solid|double|dotted|dashed)$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        (",\"7" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` (",\"7" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",\"7" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` (",\"7" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does match string" $ runTDFATest $ do
        (",\"7" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` (",\"7" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",\"/;quo&0_h" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` (",\"/;quo&0_h" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",\"/;quo&0_h" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` (",\"/;quo&0_h" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does match string" $ runTDFATest $ do
        (",\"/;quo&0_h" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` (",\"/;quo&0_h" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",<oIcjXSVV/~" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` (",<oIcjXSVV/~" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",<oIcjXSVV/~" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` (",<oIcjXSVV/~" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does match string" $ runTDFATest $ do
        (",<oIcjXSVV/~" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` (",<oIcjXSVV/~" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",xjrq\\" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` (",xjrq\\" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",xjrq\\" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` (",xjrq\\" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does match string" $ runTDFATest $ do
        (",xjrq\\" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` (",xjrq\\" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",7Y_" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` (",7Y_" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",7Y_" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` (",7Y_" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does match string" $ runTDFATest $ do
        (",7Y_" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` (",7Y_" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",ARc-C-(n" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` (",ARc-C-(n" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",ARc-C-(n" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` (",ARc-C-(n" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does match string" $ runTDFATest $ do
        (",ARc-C-(n" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` (",ARc-C-(n" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",\"%c" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` (",\"%c" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",\"%c" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` (",\"%c" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does match string" $ runTDFATest $ do
        (",\"%c" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` (",\"%c" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("aA%Fs|#=_" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` ("aA%Fs|#=_" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("aA%Fs|#=_" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` ("aA%Fs|#=_" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("aA%Fs|#=_" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` ("aA%Fs|#=_" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("V\"~" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` ("V\"~" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("V\"~" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` ("V\"~" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("V\"~" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` ("V\"~" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("V==8Pw" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` ("V==8Pw" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("V==8Pw" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` ("V==8Pw" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("V==8Pw" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` ("V==8Pw" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("?\"&;[<q^qg" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` ("?\"&;[<q^qg" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("?\"&;[<q^qg" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` ("?\"&;[<q^qg" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("?\"&;[<q^qg" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` ("?\"&;[<q^qg" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("X\"A<~." =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` ("X\"A<~." TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("X\"A<~." =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` ("X\"A<~." TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("X\"A<~." =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` ("X\"A<~." TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":\"vB8QA}L" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool) `shouldBe` (":\"vB8QA}L" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":\"vB8QA}L" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String) `shouldBe` (":\"vB8QA}L" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: String)
      it "does not match string" $ runTDFATest $ do
        (":\"vB8QA}L" =~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String)) `shouldBe` (":\"vB8QA}L" TDFA.=~ ",([\\\"]{0,1}([^\\\"]*)[^\\\"]{0,1}$)" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: Bool) `shouldBe` ("thu" TDFA.=~ "thu" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: String) `shouldBe` ("thu" TDFA.=~ "thu" :: String)
      it "does match string" $ runTDFATest $ do
        ("thu" =~ "thu" :: (String, String, String)) `shouldBe` ("thu" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("twu" =~ "thu" :: Bool) `shouldBe` ("twu" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("twu" =~ "thu" :: String) `shouldBe` ("twu" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("twu" =~ "thu" :: (String, String, String)) `shouldBe` ("twu" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("thI" =~ "thu" :: Bool) `shouldBe` ("thI" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("thI" =~ "thu" :: String) `shouldBe` ("thI" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("thI" =~ "thu" :: (String, String, String)) `shouldBe` ("thI" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("2hu" =~ "thu" :: Bool) `shouldBe` ("2hu" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("2hu" =~ "thu" :: String) `shouldBe` ("2hu" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("2hu" =~ "thu" :: (String, String, String)) `shouldBe` ("2hu" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tJu" =~ "thu" :: Bool) `shouldBe` ("tJu" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tJu" =~ "thu" :: String) `shouldBe` ("tJu" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tJu" =~ "thu" :: (String, String, String)) `shouldBe` ("tJu" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("th[" =~ "thu" :: Bool) `shouldBe` ("th[" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("th[" =~ "thu" :: String) `shouldBe` ("th[" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("th[" =~ "thu" :: (String, String, String)) `shouldBe` ("th[" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tNu" =~ "thu" :: Bool) `shouldBe` ("tNu" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tNu" =~ "thu" :: String) `shouldBe` ("tNu" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tNu" =~ "thu" :: (String, String, String)) `shouldBe` ("tNu" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("thuy" =~ "thu" :: Bool) `shouldBe` ("thuy" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("thuy" =~ "thu" :: String) `shouldBe` ("thuy" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("thuy" =~ "thu" :: (String, String, String)) `shouldBe` ("thuy" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("thV" =~ "thu" :: Bool) `shouldBe` ("thV" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("thV" =~ "thu" :: String) `shouldBe` ("thV" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("thV" =~ "thu" :: (String, String, String)) `shouldBe` ("thV" TDFA.=~ "thu" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("th]" =~ "thu" :: Bool) `shouldBe` ("th]" TDFA.=~ "thu" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("th]" =~ "thu" :: String) `shouldBe` ("th]" TDFA.=~ "thu" :: String)
      it "does not match string" $ runTDFATest $ do
        ("th]" =~ "thu" :: (String, String, String)) `shouldBe` ("th]" TDFA.=~ "thu" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("[0.377553]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[0.377553]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[0.377553]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[0.377553]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[0.377553]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[0.377553]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[666920050]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[666920050]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[666920050]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[666920050]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[666920050]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[666920050]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[.90426524]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[.90426524]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[.90426524]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[.90426524]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[.90426524]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[.90426524]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[8003511319]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[8003511319]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[8003511319]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[8003511319]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[8003511319]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[8003511319]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[4160]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[4160]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[4160]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[4160]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[4160]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[4160]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[963]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[963]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[963]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[963]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[963]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[963]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[107659.290]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[107659.290]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[107659.290]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[107659.290]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[107659.290]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[107659.290]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[414.]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[414.]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[414.]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[414.]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[414.]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[414.]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[71464]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[71464]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[71464]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[71464]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[71464]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[71464]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[560427831]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[560427831]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[560427831]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[560427831]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[560427831]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[560427831]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[8]I" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[8]I" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[8]I" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[8]I" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[8]I" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[8]I" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[6128080!]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[6128080!]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[6128080!]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[6128080!]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[6128080!]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[6128080!]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[27482.44$]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[27482.44$]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[27482.44$]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[27482.44$]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[27482.44$]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[27482.44$]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[/6440730]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[/6440730]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[/6440730]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[/6440730]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[/6440730]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[/6440730]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[U47500.168]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[U47500.168]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[U47500.168]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[U47500.168]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[U47500.168]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[U47500.168]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[.13967.4%4]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[.13967.4%4]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[.13967.4%4]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[.13967.4%4]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[.13967.4%4]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[.13967.4%4]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[17]f" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[17]f" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[17]f" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[17]f" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[17]f" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[17]f" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[4\"282]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("[4\"282]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[4\"282]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("[4\"282]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[4\"282]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("[4\"282]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("t2]" =~ "^\\[([\\d.]+)\\]" :: Bool) `shouldBe` ("t2]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("t2]" =~ "^\\[([\\d.]+)\\]" :: String) `shouldBe` ("t2]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("t2]" =~ "^\\[([\\d.]+)\\]" :: (String, String, String)) `shouldBe` ("t2]" TDFA.=~ "^\\[([[[:digit:]].]+)\\]" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("   - " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool) `shouldBe` ("   - " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("   - " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String) `shouldBe` ("   - " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String)
      it "does match string" $ runTDFATest $ do
        ("   - " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String)) `shouldBe` ("   - " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("81908. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool) `shouldBe` ("81908. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("81908. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String) `shouldBe` ("81908. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String)
      it "does match string" $ runTDFATest $ do
        ("81908. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String)) `shouldBe` ("81908. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("   1. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool) `shouldBe` ("   1. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("   1. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String) `shouldBe` ("   1. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String)
      it "does match string" $ runTDFATest $ do
        ("   1. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String)) `shouldBe` ("   1. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("+ " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool) `shouldBe` ("+ " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("+ " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String) `shouldBe` ("+ " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String)
      it "does match string" $ runTDFATest $ do
        ("+ " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String)) `shouldBe` ("+ " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("   00. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool) `shouldBe` ("   00. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("   00. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String) `shouldBe` ("   00. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String)
      it "does match string" $ runTDFATest $ do
        ("   00. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String)) `shouldBe` ("   00. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("n " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool) `shouldBe` ("n " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("n " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String) `shouldBe` ("n " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("n " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String)) `shouldBe` ("n " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("-p" =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool) `shouldBe` ("-p" TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("-p" =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String) `shouldBe` ("-p" TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("-p" =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String)) `shouldBe` ("-p" TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("   943723A. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool) `shouldBe` ("   943723A. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("   943723A. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String) `shouldBe` ("   943723A. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("   943723A. " =~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String)) `shouldBe` ("   943723A. " TDFA.=~ "^(\\t?| {0,3})([\\*\\-\\+]|[0-9]{1,9}\\.)[ \\t]" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: Bool) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: String) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: Bool) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: String) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: Bool) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: String) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: Bool) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: String) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":80" =~ ":(80|443)$" :: Bool) `shouldBe` (":80" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":80" =~ ":(80|443)$" :: String) `shouldBe` (":80" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":80" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":80" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: Bool) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: String) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":80" =~ ":(80|443)$" :: Bool) `shouldBe` (":80" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":80" =~ ":(80|443)$" :: String) `shouldBe` (":80" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":80" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":80" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: Bool) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: String) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: Bool) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: String) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: Bool) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: String) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: String)
      it "does match string" $ runTDFATest $ do
        (":443" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":443" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":f43" =~ ":(80|443)$" :: Bool) `shouldBe` (":f43" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":f43" =~ ":(80|443)$" :: String) `shouldBe` (":f43" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":f43" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":f43" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":4a3" =~ ":(80|443)$" :: Bool) `shouldBe` (":4a3" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":4a3" =~ ":(80|443)$" :: String) `shouldBe` (":4a3" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":4a3" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":4a3" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":80%" =~ ":(80|443)$" :: Bool) `shouldBe` (":80%" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":80%" =~ ":(80|443)$" :: String) `shouldBe` (":80%" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":80%" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":80%" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("]80" =~ ":(80|443)$" :: Bool) `shouldBe` ("]80" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("]80" =~ ":(80|443)$" :: String) `shouldBe` ("]80" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("]80" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` ("]80" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":81" =~ ":(80|443)$" :: Bool) `shouldBe` (":81" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":81" =~ ":(80|443)$" :: String) `shouldBe` (":81" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":81" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":81" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":44)" =~ ":(80|443)$" :: Bool) `shouldBe` (":44)" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":44)" =~ ":(80|443)$" :: String) `shouldBe` (":44)" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":44)" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":44)" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":80o" =~ ":(80|443)$" :: Bool) `shouldBe` (":80o" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":80o" =~ ":(80|443)$" :: String) `shouldBe` (":80o" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":80o" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":80o" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("%80" =~ ":(80|443)$" :: Bool) `shouldBe` ("%80" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("%80" =~ ":(80|443)$" :: String) `shouldBe` ("%80" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("%80" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` ("%80" TDFA.=~ ":(80|443)$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":809" =~ ":(80|443)$" :: Bool) `shouldBe` (":809" TDFA.=~ ":(80|443)$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":809" =~ ":(80|443)$" :: String) `shouldBe` (":809" TDFA.=~ ":(80|443)$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":809" =~ ":(80|443)$" :: (String, String, String)) `shouldBe` (":809" TDFA.=~ ":(80|443)$" :: (String, String, String))
    describe "Full matching" $ do
      it "does not match string" $ runTDFATest $ do
        ("EXEC_FLOW_JOB_PID=9737264N" =~ "EXEC_FLOW_JOB_PID=(\\d+)\\n" :: Bool) `shouldBe` ("EXEC_FLOW_JOB_PID=9737264N" TDFA.=~ "EXEC_FLOW_JOB_PID=([[:digit:]]+)\\n" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("EXEC_FLOW_JOB_PID=9737264N" =~ "EXEC_FLOW_JOB_PID=(\\d+)\\n" :: String) `shouldBe` ("EXEC_FLOW_JOB_PID=9737264N" TDFA.=~ "EXEC_FLOW_JOB_PID=([[:digit:]]+)\\n" :: String)
      it "does not match string" $ runTDFATest $ do
        ("EXEC_FLOW_JOB_PID=9737264N" =~ "EXEC_FLOW_JOB_PID=(\\d+)\\n" :: (String, String, String)) `shouldBe` ("EXEC_FLOW_JOB_PID=9737264N" TDFA.=~ "EXEC_FLOW_JOB_PID=([[:digit:]]+)\\n" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does match string" $ runTDFATest $ do
        ("BUCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("B_CH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("B_CH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("B_CH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("B_CH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("B_CH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("B_CH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("BUPH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUPH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("BUPH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUPH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("BUPH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUPH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sq" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sq" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sq" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sq" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sq" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sq" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("BUCH/s:" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/s:" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/s:" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/s:" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/s:" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/s:" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_UCH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("_UCH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_UCH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("_UCH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_UCH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("_UCH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("BUgH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUgH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("BUgH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUgH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("BUgH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUgH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sN" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sN" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sN" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sN" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sN" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sN" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("BUIH/sa" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUIH/sa" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("BUIH/sa" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUIH/sa" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("BUIH/sa" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUIH/sa" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sa{" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/sa{" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sa{" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/sa{" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/sa{" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/sa{" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("BUCH/s>" =~ "BUCH\\/sa" :: Bool) `shouldBe` ("BUCH/s>" TDFA.=~ "BUCH\\/sa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/s>" =~ "BUCH\\/sa" :: String) `shouldBe` ("BUCH/s>" TDFA.=~ "BUCH\\/sa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("BUCH/s>" =~ "BUCH\\/sa" :: (String, String, String)) `shouldBe` ("BUCH/s>" TDFA.=~ "BUCH\\/sa" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: Bool) `shouldBe` ("@@" TDFA.=~ "(@@)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: String) `shouldBe` ("@@" TDFA.=~ "(@@)" :: String)
      it "does match string" $ runTDFATest $ do
        ("@@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("@@?" =~ "(@@)" :: Bool) `shouldBe` ("@@?" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("@@?" =~ "(@@)" :: String) `shouldBe` ("@@?" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("@@?" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@?" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("@@V" =~ "(@@)" :: Bool) `shouldBe` ("@@V" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("@@V" =~ "(@@)" :: String) `shouldBe` ("@@V" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("@@V" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@@V" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("@<" =~ "(@@)" :: Bool) `shouldBe` ("@<" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("@<" =~ "(@@)" :: String) `shouldBe` ("@<" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("@<" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@<" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("@_" =~ "(@@)" :: Bool) `shouldBe` ("@_" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("@_" =~ "(@@)" :: String) `shouldBe` ("@_" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("@_" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@_" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("@j" =~ "(@@)" :: Bool) `shouldBe` ("@j" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("@j" =~ "(@@)" :: String) `shouldBe` ("@j" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("@j" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@j" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("@R" =~ "(@@)" :: Bool) `shouldBe` ("@R" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("@R" =~ "(@@)" :: String) `shouldBe` ("@R" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("@R" =~ "(@@)" :: (String, String, String)) `shouldBe` ("@R" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("J@" =~ "(@@)" :: Bool) `shouldBe` ("J@" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("J@" =~ "(@@)" :: String) `shouldBe` ("J@" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("J@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("J@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Q@" =~ "(@@)" :: Bool) `shouldBe` ("Q@" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Q@" =~ "(@@)" :: String) `shouldBe` ("Q@" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Q@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("Q@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":@" =~ "(@@)" :: Bool) `shouldBe` (":@" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":@" =~ "(@@)" :: String) `shouldBe` (":@" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        (":@" =~ "(@@)" :: (String, String, String)) `shouldBe` (":@" TDFA.=~ "(@@)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("7@" =~ "(@@)" :: Bool) `shouldBe` ("7@" TDFA.=~ "(@@)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("7@" =~ "(@@)" :: String) `shouldBe` ("7@" TDFA.=~ "(@@)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("7@" =~ "(@@)" :: (String, String, String)) `shouldBe` ("7@" TDFA.=~ "(@@)" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: Bool) `shouldBe` ("$director" TDFA.=~ "\\$director" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: String) `shouldBe` ("$director" TDFA.=~ "\\$director" :: String)
      it "does match string" $ runTDFATest $ do
        ("$director" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$director" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$direstor" =~ "\\$director" :: Bool) `shouldBe` ("$direstor" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$direstor" =~ "\\$director" :: String) `shouldBe` ("$direstor" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$direstor" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$direstor" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$Girector" =~ "\\$director" :: Bool) `shouldBe` ("$Girector" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$Girector" =~ "\\$director" :: String) `shouldBe` ("$Girector" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$Girector" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$Girector" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$direstor" =~ "\\$director" :: Bool) `shouldBe` ("$direstor" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$direstor" =~ "\\$director" :: String) `shouldBe` ("$direstor" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$direstor" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$direstor" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$directoj" =~ "\\$director" :: Bool) `shouldBe` ("$directoj" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$directoj" =~ "\\$director" :: String) `shouldBe` ("$directoj" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$directoj" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$directoj" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$diXector" =~ "\\$director" :: Bool) `shouldBe` ("$diXector" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$diXector" =~ "\\$director" :: String) `shouldBe` ("$diXector" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$diXector" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$diXector" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$direc\\or" =~ "\\$director" :: Bool) `shouldBe` ("$direc\\or" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$direc\\or" =~ "\\$director" :: String) `shouldBe` ("$direc\\or" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$direc\\or" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$direc\\or" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$directob" =~ "\\$director" :: Bool) `shouldBe` ("$directob" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$directob" =~ "\\$director" :: String) `shouldBe` ("$directob" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$directob" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$directob" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$direcDor" =~ "\\$director" :: Bool) `shouldBe` ("$direcDor" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$direcDor" =~ "\\$director" :: String) `shouldBe` ("$direcDor" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$direcDor" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$direcDor" TDFA.=~ "\\$director" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$dirZctor" =~ "\\$director" :: Bool) `shouldBe` ("$dirZctor" TDFA.=~ "\\$director" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$dirZctor" =~ "\\$director" :: String) `shouldBe` ("$dirZctor" TDFA.=~ "\\$director" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$dirZctor" =~ "\\$director" :: (String, String, String)) `shouldBe` ("$dirZctor" TDFA.=~ "\\$director" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("]" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("]" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("]" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("]" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("]" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("]" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("|" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("|" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("|" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("|" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("^" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("^" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("^" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("^" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("^" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("^" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("-" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("-" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("-" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("-" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("$" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("$" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("$" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("#" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("#" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("#" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("#" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("#" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("#" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("/" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("/" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("/" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("/" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("/" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("/" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("-" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("-" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("-" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("-" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("^" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("^" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("^" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("^" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("^" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("^" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("}" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("}" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("}" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("=" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("=" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("=" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("=" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("=" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("=" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|g" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("|g" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|g" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("|g" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|g" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("|g" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|[" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("|[" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|[" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("|[" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|[" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("|[" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|," =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("|," TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|," =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("|," TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|," =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("|," TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("'" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("'" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("'" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("'" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("'" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("'" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("'" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("'" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("'" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("'" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("'" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("'" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("U" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("U" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("U" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("U" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("U" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("U" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("\"" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("\"" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("\"" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("\"" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("\"" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("\"" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("j" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool) `shouldBe` ("j" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("j" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String) `shouldBe` ("j" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("j" =~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String)) `shouldBe` ("j" TDFA.=~ "[\\-\\[\\]\\/\\{\\}\\(\\)\\+\\.\\\\\\^\\$\\|\\,\\#]" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("]" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("]" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("]" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("]" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        ("]" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("]" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("$" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("$" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("$" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("^" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("^" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("^" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("^" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        ("^" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("^" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("\"" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("\"" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\"" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("\"" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        ("\"" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("\"" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("(" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("(" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("(" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("(" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        ("(" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("(" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("/" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("/" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("/" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("/" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        ("/" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("/" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` (":" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` (":" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        (":" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` (":" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("=" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("=" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("=" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("=" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        ("=" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("=" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("*" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("*" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("*" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("*" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        ("*" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("*" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (")" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` (")" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does match string" $ runTDFATest $ do
        (")" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` (")" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does match string" $ runTDFATest $ do
        (")" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` (")" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$V" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("$V" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$V" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("$V" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$V" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("$V" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("p" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("p" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("p" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("p" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("p" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("p" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("g" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("g" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("g" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("g" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("g" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("g" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (">j" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` (">j" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (">j" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` (">j" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        (">j" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` (">j" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("1" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("1" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("1" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("1" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("1" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("1" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("@r" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("@r" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("@r" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("@r" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("@r" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("@r" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$x" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("$x" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$x" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("$x" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$x" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("$x" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("W" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("W" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("W" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("W" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("W" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("W" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("]5" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("]5" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("]5" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("]5" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("]5" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("]5" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool) `shouldBe` ("f" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String) `shouldBe` ("f" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String)) `shouldBe` ("f" TDFA.=~ "^[\\[\\];:?()!.,><=+-/*|&@^%\"'$~]" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("2782.3" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("2782.3" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("2782.3" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("2782.3" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("2782.3" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("2782.3" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-44317359.073347382" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("-44317359.073347382" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-44317359.073347382" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("-44317359.073347382" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("-44317359.073347382" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("-44317359.073347382" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("+36912" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("+36912" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("+36912" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("+36912" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("+36912" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("+36912" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("vlibgtzbwj" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("vlibgtzbwj" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("vlibgtzbwj" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("vlibgtzbwj" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("vlibgtzbwj" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("vlibgtzbwj" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("r" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("r" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("r" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("r" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("r" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("r" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("frlku" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("frlku" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("frlku" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("frlku" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("frlku" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("frlku" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("2228" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("2228" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("2228" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("2228" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("2228" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("2228" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (".177934" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` (".177934" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        (".177934" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` (".177934" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        (".177934" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` (".177934" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("31882773.7" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("31882773.7" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("31882773.7" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("31882773.7" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("31882773.7" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("31882773.7" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("dwapfoans" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("dwapfoans" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("dwapfoans" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("dwapfoans" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("dwapfoans" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("dwapfoans" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("vKu" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("vKu" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("vKu" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("vKu" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("vKu" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("vKu" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("zdphzmaklB" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("zdphzmaklB" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("zdphzmaklB" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("zdphzmaklB" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("zdphzmaklB" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("zdphzmaklB" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("B" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("B" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("B" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("B" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("B" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("B" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("3k3" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("3k3" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("3k3" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("3k3" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("3k3" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("3k3" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("o$" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("o$" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("o$" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("o$" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("o$" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("o$" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("m2" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("m2" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("m2" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("m2" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("m2" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("m2" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("hd)" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("hd)" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("hd)" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("hd)" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("hd)" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("hd)" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Rbkiqrlb" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("Rbkiqrlb" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Rbkiqrlb" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("Rbkiqrlb" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Rbkiqrlb" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("Rbkiqrlb" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("-q766" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("-q766" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("-q766" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("-q766" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("-q766" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("-q766" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("wdaz$dhrv" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: Bool) `shouldBe` ("wdaz$dhrv" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("wdaz$dhrv" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: String) `shouldBe` ("wdaz$dhrv" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("wdaz$dhrv" =~ "[\\+\\-]?(\\d*\\.\\d+|\\d+)|[a-z]+" :: (String, String, String)) `shouldBe` ("wdaz$dhrv" TDFA.=~ "[\\+\\-]?([[:digit:]]*\\.[[:digit:]]+|[[:digit:]]+)|[a-z]+" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("cannot query field-?updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field-?updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("cannot query field-?updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field-?updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does match string" $ runTDFATest $ do
        ("cannot query field-?updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field-?updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("cannot query field1H+'_%CQ)updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field1H+'_%CQ)updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("cannot query field1H+'_%CQ)updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field1H+'_%CQ)updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does match string" $ runTDFATest $ do
        ("cannot query field1H+'_%CQ)updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field1H+'_%CQ)updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("cannot query field&updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field&updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("cannot query field&updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field&updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does match string" $ runTDFATest $ do
        ("cannot query field&updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field&updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("cannot query field2s;99updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field2s;99updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("cannot query field2s;99updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field2s;99updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does match string" $ runTDFATest $ do
        ("cannot query field2s;99updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field2s;99updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("cannot query field%<8s\"updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field%<8s\"updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("cannot query field%<8s\"updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field%<8s\"updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does match string" $ runTDFATest $ do
        ("cannot query field%<8s\"updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field%<8s\"updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("cannot query field/0(q>:/j]updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field/0(q>:/j]updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("cannot query field/0(q>:/j]updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field/0(q>:/j]updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does match string" $ runTDFATest $ do
        ("cannot query field/0(q>:/j]updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field/0(q>:/j]updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("cannot query fieldU#H:updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query fieldU#H:updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("cannot query fieldU#H:updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query fieldU#H:updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does match string" $ runTDFATest $ do
        ("cannot query fieldU#H:updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query fieldU#H:updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cannot query field; da6K\\.up&ateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field; da6K\\.up&ateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cannot query field; da6K\\.up&ateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field; da6K\\.up&ateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cannot query field; da6K\\.up&ateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field; da6K\\.up&ateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cannot q-ery field}7updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot q-ery field}7updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cannot q-ery field}7updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot q-ery field}7updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cannot q-ery field}7updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot q-ery field}7updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cannot query field<EeY64gupdateReaYonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field<EeY64gupdateReaYonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cannot query field<EeY64gupdateReaYonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field<EeY64gupdateReaYonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cannot query field<EeY64gupdateReaYonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field<EeY64gupdateReaYonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cannot que!y field#Evs,9(x,updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot que!y field#Evs,9(x,updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cannot que!y field#Evs,9(x,updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot que!y field#Evs,9(x,updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cannot que!y field#Evs,9(x,updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot que!y field#Evs,9(x,updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cannos query field\"F&i*zupdateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannos query field\"F&i*zupdateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cannos query field\"F&i*zupdateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannos query field\"F&i*zupdateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cannos query field\"F&i*zupdateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannos query field\"F&i*zupdateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cannot query field0JUSwuGupdateReadonlyTal" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot query field0JUSwuGupdateReadonlyTal" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cannot query field0JUSwuGupdateReadonlyTal" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot query field0JUSwuGupdateReadonlyTal" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cannot query field0JUSwuGupdateReadonlyTal" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot query field0JUSwuGupdateReadonlyTal" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cannot queryLfieldT^.=)5o/updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot queryLfieldT^.=)5o/updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cannot queryLfieldT^.=)5o/updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot queryLfieldT^.=)5o/updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cannot queryLfieldT^.=)5o/updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot queryLfieldT^.=)5o/updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("cannot qPery field<d-updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: Bool) `shouldBe` ("cannot qPery field<d-updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("cannot qPery field<d-updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: String) `shouldBe` ("cannot qPery field<d-updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: String)
      it "does not match string" $ runTDFATest $ do
        ("cannot qPery field<d-updateReadonlyTag" =~ "cannot query field.+updateReadonlyTag" :: (String, String, String)) `shouldBe` ("cannot qPery field<d-updateReadonlyTag" TDFA.=~ "cannot query field.+updateReadonlyTag" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("ReaderZask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("ReaderZask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ReaderZask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("ReaderZask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("ReaderZask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("ReaderZask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Readerjask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Readerjask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Readerjask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Readerjask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("Readerjask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Readerjask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Reader#ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Reader#ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Reader#ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Reader#ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("Reader#ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Reader#ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Reader;ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Reader;ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Reader;ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Reader;ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("Reader;ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Reader;ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Readeruask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Readeruask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Readeruask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Readeruask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("Readeruask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Readeruask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ReaderGask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("ReaderGask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ReaderGask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("ReaderGask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("ReaderGask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("ReaderGask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Reader8ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Reader8ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Reader8ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Reader8ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("Reader8ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Reader8ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Readervask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Readervask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Readervask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Readervask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("Readervask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Readervask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Reader)ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Reader)ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Reader)ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Reader)ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("Reader)ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Reader)ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Readercask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Readercask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Readercask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Readercask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does match string" $ runTDFATest $ do
        ("Readercask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Readercask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Readergask: No aHgument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Readergask: No aHgument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Readergask: No aHgument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Readergask: No aHgument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Readergask: No aHgument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Readergask: No aHgument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Readerhask: No argu^ent or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Readerhask: No argu^ent or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Readerhask: No argu^ent or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Readerhask: No argu^ent or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Readerhask: No argu^ent or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Readerhask: No argu^ent or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ReaRerzask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("ReaRerzask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ReaRerzask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("ReaRerzask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ReaRerzask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("ReaRerzask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Reader/ask: No argum{nt or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Reader/ask: No argum{nt or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Reader/ask: No argum{nt or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Reader/ask: No argum{nt or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Reader/ask: No argum{nt or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Reader/ask: No argum{nt or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("R:ader3ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("R:ader3ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("R:ader3ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("R:ader3ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("R:ader3ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("R:ader3ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Re:der2ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Re:der2ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Re:der2ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Re:der2ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Re:der2ask: No argument or function required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Re:der2ask: No argument or function required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Readeryask: No argument or functTon required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Readeryask: No argument or functTon required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Readeryask: No argument or functTon required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Readeryask: No argument or functTon required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Readeryask: No argument or functTon required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Readeryask: No argument or functTon required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Reader'ask: No argument or function reqmired" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Reader'ask: No argument or function reqmired" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Reader'ask: No argument or function reqmired" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Reader'ask: No argument or function reqmired" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Reader'ask: No argument or function reqmired" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Reader'ask: No argument or function reqmired" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Readersask: No argument or kunction required" =~ "Reader.ask: No argument or function required" :: Bool) `shouldBe` ("Readersask: No argument or kunction required" TDFA.=~ "Reader.ask: No argument or function required" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Readersask: No argument or kunction required" =~ "Reader.ask: No argument or function required" :: String) `shouldBe` ("Readersask: No argument or kunction required" TDFA.=~ "Reader.ask: No argument or function required" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Readersask: No argument or kunction required" =~ "Reader.ask: No argument or function required" :: (String, String, String)) `shouldBe` ("Readersask: No argument or kunction required" TDFA.=~ "Reader.ask: No argument or function required" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can nTt be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can nTt be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can nTt be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can nTt be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can nTt be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can nTt be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can n9t be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can n9t be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can n9t be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can n9t be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can n9t be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can n9t be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node5'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node5'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node5'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node5'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node5'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node5'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node 'schebe' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'schebe' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node 'schebe' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'schebe' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node 'schebe' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'schebe' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("No{e 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("No{e 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("No{e 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("No{e 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("No{e 'scheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("No{e 'scheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can not b& a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not b& a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can not b& a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not b& a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can not b& a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not b& a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node 'ocheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'ocheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node 'ocheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'ocheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node 'ocheme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'ocheme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node 'sczeme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'sczeme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node 'sczeme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'sczeme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node 'sczeme' can not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'sczeme' can not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar9" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' can not be a scalar9" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar9" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' can not be a scalar9" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' can not be a scalar9" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' can not be a scalar9" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' c3n not be a scalar" =~ "Node 'scheme' can not be a scalar" :: Bool) `shouldBe` ("Node 'scheme' c3n not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' c3n not be a scalar" =~ "Node 'scheme' can not be a scalar" :: String) `shouldBe` ("Node 'scheme' c3n not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Node 'scheme' c3n not be a scalar" =~ "Node 'scheme' can not be a scalar" :: (String, String, String)) `shouldBe` ("Node 'scheme' c3n not be a scalar" TDFA.=~ "Node 'scheme' can not be a scalar" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("|R4|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|R4|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|R4|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|R4|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|R4|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|R4|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|)|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|)|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|)|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|)|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|)|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|)|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|8SkJD00R|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|8SkJD00R|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|8SkJD00R|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|8SkJD00R|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|8SkJD00R|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|8SkJD00R|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|VGn|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|VGn|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|VGn|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|VGn|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|VGn|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|VGn|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|340nF)gtSm|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|340nF)gtSm|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|340nF)gtSm|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|340nF)gtSm|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|340nF)gtSm|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|340nF)gtSm|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|9RGcc|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|9RGcc|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|9RGcc|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|9RGcc|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|9RGcc|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|9RGcc|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|J2Gj|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|J2Gj|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|J2Gj|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|J2Gj|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|J2Gj|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|J2Gj|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|aeXqK0|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|aeXqK0|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|aeXqK0|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|aeXqK0|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|aeXqK0|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|aeXqK0|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|(IWo2wiw)N|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|(IWo2wiw)N|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|(IWo2wiw)N|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|(IWo2wiw)N|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|(IWo2wiw)N|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|(IWo2wiw)N|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("|W|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|W|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("|W|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|W|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does match string" $ runTDFATest $ do
        ("|W|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|W|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("5b|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("5b|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("5b|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("5b|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("5b|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("5b|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|KxUytDUr" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|KxUytDUr" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|KxUytDUr" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|KxUytDUr" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|KxUytDUr" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|KxUytDUr" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|k\"sa4zI(Z|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|k\"sa4zI(Z|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|k\"sa4zI(Z|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|k\"sa4zI(Z|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|k\"sa4zI(Z|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|k\"sa4zI(Z|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|Ywo$8G|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|Ywo$8G|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|Ywo$8G|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|Ywo$8G|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|Ywo$8G|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|Ywo$8G|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|KDDRM.+|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|KDDRM.+|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|KDDRM.+|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|KDDRM.+|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|KDDRM.+|_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|KDDRM.+|_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|bZxDCHM0.GL_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|bZxDCHM0.GL_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|bZxDCHM0.GL_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|bZxDCHM0.GL_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|bZxDCHM0.GL_" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|bZxDCHM0.GL_" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|Z'4ddZ9l|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool) `shouldBe` ("|Z'4ddZ9l|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|Z'4ddZ9l|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String) `shouldBe` ("|Z'4ddZ9l|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|Z'4ddZ9l|" =~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String)) `shouldBe` ("|Z'4ddZ9l|" TDFA.=~ "\\|([a-zA-Z0-9.()]+)\\|_?" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::[*9E ;" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::[*9E ;" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::[*9E ;" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::[*9E ;" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::[*9E ;" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::[*9E ;" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::%;]q1" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::%;]q1" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::%;]q1" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::%;]q1" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::%;]q1" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::%;]q1" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::x{f" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::x{f" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::x{f" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::x{f" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::x{f" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::x{f" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::LSfd:SD-Y/2" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::LSfd:SD-Y/2" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::LSfd:SD-Y/2" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::LSfd:SD-Y/2" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::LSfd:SD-Y/2" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::LSfd:SD-Y/2" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::Dd@YA&%|A:]." =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::Dd@YA&%|A:]." TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::Dd@YA&%|A:]." =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::Dd@YA&%|A:]." TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::Dd@YA&%|A:]." =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::Dd@YA&%|A:]." TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::1:\\oO" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::1:\\oO" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::1:\\oO" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::1:\\oO" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::1:\\oO" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::1:\\oO" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::tCA" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::tCA" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::tCA" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::tCA" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::tCA" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::tCA" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::G>d|8" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::G>d|8" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::G>d|8" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::G>d|8" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::G>d|8" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::G>d|8" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::Ca~=f?nI?" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-self::Ca~=f?nI?" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::Ca~=f?nI?" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-self::Ca~=f?nI?" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ancestor-or-self::Ca~=f?nI?" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-self::Ca~=f?nI?" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-s]lf::Dn." =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-s]lf::Dn." TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-s]lf::Dn." =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-s]lf::Dn." TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-s]lf::Dn." =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-s]lf::Dn." TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-selY::t" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-selY::t" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-selY::t" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-selY::t" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-selY::t" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-selY::t" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("anOestor-or-self::(:" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("anOestor-or-self::(:" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("anOestor-or-self::(:" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("anOestor-or-self::(:" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("anOestor-or-self::(:" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("anOestor-or-self::(:" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-oelf::Fx!c" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-oelf::Fx!c" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-oelf::Fx!c" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-oelf::Fx!c" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-oelf::Fx!c" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-oelf::Fx!c" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ancestor-o1-self::aA>|]6D" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-o1-self::aA>|]6D" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-o1-self::aA>|]6D" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-o1-self::aA>|]6D" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-o1-self::aA>|]6D" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-o1-self::aA>|]6D" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-nelf::7Hy" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool) `shouldBe` ("ancestor-or-nelf::7Hy" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-nelf::7Hy" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String) `shouldBe` ("ancestor-or-nelf::7Hy" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ancestor-or-nelf::7Hy" =~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String)) `shouldBe` ("ancestor-or-nelf::7Hy" TDFA.=~ "ancestor-or-self::([^\\:]*)(\\:(.*)){0,1}$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("zkmj-dnm" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("zkmj-dnm" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("zkmj-dnm" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("zkmj-dnm" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("zkmj-dnm" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("zkmj-dnm" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("jodt-fye-qlkwbdzbpa-mmwxpcan" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("jodt-fye-qlkwbdzbpa-mmwxpcan" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("jodt-fye-qlkwbdzbpa-mmwxpcan" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("jodt-fye-qlkwbdzbpa-mmwxpcan" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("jodt-fye-qlkwbdzbpa-mmwxpcan" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("jodt-fye-qlkwbdzbpa-mmwxpcan" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("vmxtwyhe-xjkhpv-apwxnkjxek-b-zpyklftlx-ywl-e-qnsrrf-twxlirnfb" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("vmxtwyhe-xjkhpv-apwxnkjxek-b-zpyklftlx-ywl-e-qnsrrf-twxlirnfb" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("vmxtwyhe-xjkhpv-apwxnkjxek-b-zpyklftlx-ywl-e-qnsrrf-twxlirnfb" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("vmxtwyhe-xjkhpv-apwxnkjxek-b-zpyklftlx-ywl-e-qnsrrf-twxlirnfb" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("vmxtwyhe-xjkhpv-apwxnkjxek-b-zpyklftlx-ywl-e-qnsrrf-twxlirnfb" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("vmxtwyhe-xjkhpv-apwxnkjxek-b-zpyklftlx-ywl-e-qnsrrf-twxlirnfb" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("icgfbrf-hbsc-tfkkpga-pimmrmusz-opwvu-a-rgny" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("icgfbrf-hbsc-tfkkpga-pimmrmusz-opwvu-a-rgny" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("icgfbrf-hbsc-tfkkpga-pimmrmusz-opwvu-a-rgny" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("icgfbrf-hbsc-tfkkpga-pimmrmusz-opwvu-a-rgny" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("icgfbrf-hbsc-tfkkpga-pimmrmusz-opwvu-a-rgny" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("icgfbrf-hbsc-tfkkpga-pimmrmusz-opwvu-a-rgny" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("aflzxe-tirmacbqog-hzibetjsx" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("aflzxe-tirmacbqog-hzibetjsx" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("aflzxe-tirmacbqog-hzibetjsx" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("aflzxe-tirmacbqog-hzibetjsx" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("aflzxe-tirmacbqog-hzibetjsx" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("aflzxe-tirmacbqog-hzibetjsx" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("ywtfi-h-rafewfkulm-jwv-gifhmy-ejx-apbwh-iwvicyp" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("ywtfi-h-rafewfkulm-jwv-gifhmy-ejx-apbwh-iwvicyp" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("ywtfi-h-rafewfkulm-jwv-gifhmy-ejx-apbwh-iwvicyp" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("ywtfi-h-rafewfkulm-jwv-gifhmy-ejx-apbwh-iwvicyp" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("ywtfi-h-rafewfkulm-jwv-gifhmy-ejx-apbwh-iwvicyp" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("ywtfi-h-rafewfkulm-jwv-gifhmy-ejx-apbwh-iwvicyp" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("y-fnxvinm--afym-axxqrbl--yd-gsnzonmf-rprhbgdb-lktjizns" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("y-fnxvinm--afym-axxqrbl--yd-gsnzonmf-rprhbgdb-lktjizns" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("y-fnxvinm--afym-axxqrbl--yd-gsnzonmf-rprhbgdb-lktjizns" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("y-fnxvinm--afym-axxqrbl--yd-gsnzonmf-rprhbgdb-lktjizns" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("y-fnxvinm--afym-axxqrbl--yd-gsnzonmf-rprhbgdb-lktjizns" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("y-fnxvinm--afym-axxqrbl--yd-gsnzonmf-rprhbgdb-lktjizns" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("bqq--sr-zqsnfmfvgr-bsavaqrexe-yyq-jppztand-yyaw-uzwg-l" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("bqq--sr-zqsnfmfvgr-bsavaqrexe-yyq-jppztand-yyaw-uzwg-l" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("bqq--sr-zqsnfmfvgr-bsavaqrexe-yyq-jppztand-yyaw-uzwg-l" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("bqq--sr-zqsnfmfvgr-bsavaqrexe-yyq-jppztand-yyaw-uzwg-l" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("bqq--sr-zqsnfmfvgr-bsavaqrexe-yyq-jppztand-yyaw-uzwg-l" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("bqq--sr-zqsnfmfvgr-bsavaqrexe-yyq-jppztand-yyaw-uzwg-l" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("xxgwhm-cgdblqoxh" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("xxgwhm-cgdblqoxh" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("xxgwhm-cgdblqoxh" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("xxgwhm-cgdblqoxh" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("xxgwhm-cgdblqoxh" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("xxgwhm-cgdblqoxh" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("yifwvycqi--wabaq-zcwtejvkq" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("yifwvycqi--wabaq-zcwtejvkq" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("yifwvycqi--wabaq-zcwtejvkq" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("yifwvycqi--wabaq-zcwtejvkq" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("yifwvycqi--wabaq-zcwtejvkq" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("yifwvycqi--wabaq-zcwtejvkq" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("vlo>ja-" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("vlo>ja-" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("vlo>ja-" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("vlo>ja-" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("vlo>ja-" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("vlo>ja-" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("a-ubxaouumd-fyuyvtqdn-cnnvyzxb-vlrpjlfc@-fwmuqd-ltzm-ruoxhyxcug" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("a-ubxaouumd-fyuyvtqdn-cnnvyzxb-vlrpjlfc@-fwmuqd-ltzm-ruoxhyxcug" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("a-ubxaouumd-fyuyvtqdn-cnnvyzxb-vlrpjlfc@-fwmuqd-ltzm-ruoxhyxcug" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("a-ubxaouumd-fyuyvtqdn-cnnvyzxb-vlrpjlfc@-fwmuqd-ltzm-ruoxhyxcug" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("a-ubxaouumd-fyuyvtqdn-cnnvyzxb-vlrpjlfc@-fwmuqd-ltzm-ruoxhyxcug" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("a-ubxaouumd-fyuyvtqdn-cnnvyzxb-vlrpjlfc@-fwmuqd-ltzm-ruoxhyxcug" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("vxydnmv-Cwcph-m-zlefbocx-nenler" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("vxydnmv-Cwcph-m-zlefbocx-nenler" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("vxydnmv-Cwcph-m-zlefbocx-nenler" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("vxydnmv-Cwcph-m-zlefbocx-nenler" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("vxydnmv-Cwcph-m-zlefbocx-nenler" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("vxydnmv-Cwcph-m-zlefbocx-nenler" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("4guexfbik-sw" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("4guexfbik-sw" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("4guexfbik-sw" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("4guexfbik-sw" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("4guexfbik-sw" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("4guexfbik-sw" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("yxmaclfxke-ntvku-u-qtevw-iptd-grs:ew" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("yxmaclfxke-ntvku-u-qtevw-iptd-grs:ew" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("yxmaclfxke-ntvku-u-qtevw-iptd-grs:ew" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("yxmaclfxke-ntvku-u-qtevw-iptd-grs:ew" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("yxmaclfxke-ntvku-u-qtevw-iptd-grs:ew" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("yxmaclfxke-ntvku-u-qtevw-iptd-grs:ew" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("vzzpljja--ln-ljobbt-djnwbu=q-dvbyam-rphhbdwuyc-jbum-zen-l-a" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("vzzpljja--ln-ljobbt-djnwbu=q-dvbyam-rphhbdwuyc-jbum-zen-l-a" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("vzzpljja--ln-ljobbt-djnwbu=q-dvbyam-rphhbdwuyc-jbum-zen-l-a" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("vzzpljja--ln-ljobbt-djnwbu=q-dvbyam-rphhbdwuyc-jbum-zen-l-a" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("vzzpljja--ln-ljobbt-djnwbu=q-dvbyam-rphhbdwuyc-jbum-zen-l-a" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("vzzpljja--ln-ljobbt-djnwbu=q-dvbyam-rphhbdwuyc-jbum-zen-l-a" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("gxBpzoo-k" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("gxBpzoo-k" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("gxBpzoo-k" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("gxBpzoo-k" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("gxBpzoo-k" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("gxBpzoo-k" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("nh-findbtqee-imqbwuvet-lgo-y--cm-wfnd-krml-zDk" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("nh-findbtqee-imqbwuvet-lgo-y--cm-wfnd-krml-zDk" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("nh-findbtqee-imqbwuvet-lgo-y--cm-wfnd-krml-zDk" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("nh-findbtqee-imqbwuvet-lgo-y--cm-wfnd-krml-zDk" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("nh-findbtqee-imqbwuvet-lgo-y--cm-wfnd-krml-zDk" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("nh-findbtqee-imqbwuvet-lgo-y--cm-wfnd-krml-zDk" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("-pi-zrvq-qziglqwogt-jfmcug-ypluxb}eex" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("-pi-zrvq-qziglqwogt-jfmcug-ypluxb}eex" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("-pi-zrvq-qziglqwogt-jfmcug-ypluxb}eex" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("-pi-zrvq-qziglqwogt-jfmcug-ypluxb}eex" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("-pi-zrvq-qziglqwogt-jfmcug-ypluxb}eex" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("-pi-zrvq-qziglqwogt-jfmcug-ypluxb}eex" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("x-l2hmyvvehy-qcbfuyjzr--z-jcshnmfkh--w-cmbwt-dudybsjdt-gfwx" =~ "^([a-z]*((-)))+[a-z]*$" :: Bool) `shouldBe` ("x-l2hmyvvehy-qcbfuyjzr--z-jcshnmfkh--w-cmbwt-dudybsjdt-gfwx" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("x-l2hmyvvehy-qcbfuyjzr--z-jcshnmfkh--w-cmbwt-dudybsjdt-gfwx" =~ "^([a-z]*((-)))+[a-z]*$" :: String) `shouldBe` ("x-l2hmyvvehy-qcbfuyjzr--z-jcshnmfkh--w-cmbwt-dudybsjdt-gfwx" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("x-l2hmyvvehy-qcbfuyjzr--z-jcshnmfkh--w-cmbwt-dudybsjdt-gfwx" =~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String)) `shouldBe` ("x-l2hmyvvehy-qcbfuyjzr--z-jcshnmfkh--w-cmbwt-dudybsjdt-gfwx" TDFA.=~ "^([a-z]*((-)))+[a-z]*$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationuparam expect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationuparam expect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationuparam expect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationuparam expect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationuparam expect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationuparam expect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationBparam expect) an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationBparam expect) an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationBparam expect) an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationBparam expect) an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationBparam expect) an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationBparam expect) an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationvparam expectV!f$6t]\\ an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationvparam expectV!f$6t]\\ an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationvparam expectV!f$6t]\\ an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationvparam expectV!f$6t]\\ an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationvparam expectV!f$6t]\\ an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationvparam expectV!f$6t]\\ an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationMparam expectA an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationMparam expectA an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationMparam expectA an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationMparam expectA an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationMparam expectA an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationMparam expectA an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation>param expecto@'7 an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validation>param expecto@'7 an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation>param expecto@'7 an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validation>param expecto@'7 an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation>param expecto@'7 an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validation>param expecto@'7 an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation4param expect;1QzP9,5Y an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validation4param expect;1QzP9,5Y an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation4param expect;1QzP9,5Y an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validation4param expect;1QzP9,5Y an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation4param expect;1QzP9,5Y an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validation4param expect;1QzP9,5Y an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationcparam expect}E[ an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationcparam expect}E[ an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationcparam expect}E[ an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationcparam expect}E[ an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationcparam expect}E[ an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationcparam expect}E[ an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationuparam expectgxkh! an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationuparam expectgxkh! an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationuparam expectgxkh! an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationuparam expectgxkh! an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationuparam expectgxkh! an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationuparam expectgxkh! an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation&param expect2Q|a& an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validation&param expect2Q|a& an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation&param expect2Q|a& an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validation&param expect2Q|a& an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation&param expect2Q|a& an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validation&param expect2Q|a& an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation_param expect)Di2t an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validation_param expect)Di2t an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation_param expect)Di2t an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validation_param expect)Di2t an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validation_param expect)Di2t an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validation_param expect)Di2t an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[byo]: test_validation*param expect(bPIm\"i7 an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[byo]: test_validation*param expect(bPIm\"i7 an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[byo]: test_validation*param expect(bPIm\"i7 an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[byo]: test_validation*param expect(bPIm\"i7 an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[byo]: test_validation*param expect(bPIm\"i7 an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[byo]: test_validation*param expect(bPIm\"i7 an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[yye]: test_validation[param expect. an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[yye]: test_validation[param expect. an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[yye]: test_validation[param expect. an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[yye]: test_validation[param expect. an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[yye]: test_validation[param expect. an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[yye]: test_validation[param expect. an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationRpasam expecte an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationRpasam expecte an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationRpasam expecte an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationRpasam expecte an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationRpasam expecte an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationRpasam expecte an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[byo]: test_validation0param expect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[byo]: test_validation0param expect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[byo]: test_validation0param expect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[byo]: test_validation0param expect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[byo]: test_validation0param expect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[byo]: test_validation0param expect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationiparam expectR#V3 an Integer oalue, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationiparam expectR#V3 an Integer oalue, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationiparam expectR#V3 an Integer oalue, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationiparam expectR#V3 an Integer oalue, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationiparam expectR#V3 an Integer oalue, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationiparam expectR#V3 an Integer oalue, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Test_vaRidation[bye]: test_validation6param expectHtTWU!n an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_vaRidation[bye]: test_validation6param expectHtTWU!n an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Test_vaRidation[bye]: test_validation6param expectHtTWU!n an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_vaRidation[bye]: test_validation6param expectHtTWU!n an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Test_vaRidation[bye]: test_validation6param expectHtTWU!n an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_vaRidation[bye]: test_validation6param expectHtTWU!n an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationeparam exVect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool) `shouldBe` ("Test_validation[bye]: test_validationeparam exVect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationeparam exVect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String) `shouldBe` ("Test_validation[bye]: test_validationeparam exVect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Test_validation[bye]: test_validationeparam exVect an Integer value, got String" =~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String)) `shouldBe` ("Test_validation[bye]: test_validationeparam exVect an Integer value, got String" TDFA.=~ "Test_validation\\[bye\\]: test_validation.param expect.* an Integer value, got String" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: Bool) `shouldBe` ("Hana" TDFA.=~ "Hana" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: String) `shouldBe` ("Hana" TDFA.=~ "Hana" :: String)
      it "does match string" $ runTDFATest $ do
        ("Hana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Ha|a" =~ "Hana" :: Bool) `shouldBe` ("Ha|a" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Ha|a" =~ "Hana" :: String) `shouldBe` ("Ha|a" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Ha|a" =~ "Hana" :: (String, String, String)) `shouldBe` ("Ha|a" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Rana" =~ "Hana" :: Bool) `shouldBe` ("Rana" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Rana" =~ "Hana" :: String) `shouldBe` ("Rana" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Rana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Rana" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Kana" =~ "Hana" :: Bool) `shouldBe` ("Kana" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Kana" =~ "Hana" :: String) `shouldBe` ("Kana" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Kana" =~ "Hana" :: (String, String, String)) `shouldBe` ("Kana" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("H2na" =~ "Hana" :: Bool) `shouldBe` ("H2na" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("H2na" =~ "Hana" :: String) `shouldBe` ("H2na" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("H2na" =~ "Hana" :: (String, String, String)) `shouldBe` ("H2na" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Hana|" =~ "Hana" :: Bool) `shouldBe` ("Hana|" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Hana|" =~ "Hana" :: String) `shouldBe` ("Hana|" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Hana|" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana|" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Hana_" =~ "Hana" :: Bool) `shouldBe` ("Hana_" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Hana_" =~ "Hana" :: String) `shouldBe` ("Hana_" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Hana_" =~ "Hana" :: (String, String, String)) `shouldBe` ("Hana_" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("jana" =~ "Hana" :: Bool) `shouldBe` ("jana" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("jana" =~ "Hana" :: String) `shouldBe` ("jana" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("jana" =~ "Hana" :: (String, String, String)) `shouldBe` ("jana" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Ha5a" =~ "Hana" :: Bool) `shouldBe` ("Ha5a" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Ha5a" =~ "Hana" :: String) `shouldBe` ("Ha5a" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Ha5a" =~ "Hana" :: (String, String, String)) `shouldBe` ("Ha5a" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("H&na" =~ "Hana" :: Bool) `shouldBe` ("H&na" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("H&na" =~ "Hana" :: String) `shouldBe` ("H&na" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("H&na" =~ "Hana" :: (String, String, String)) `shouldBe` ("H&na" TDFA.=~ "Hana" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Han!" =~ "Hana" :: Bool) `shouldBe` ("Han!" TDFA.=~ "Hana" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Han!" =~ "Hana" :: String) `shouldBe` ("Han!" TDFA.=~ "Hana" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Han!" =~ "Hana" :: (String, String, String)) `shouldBe` ("Han!" TDFA.=~ "Hana" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does match string" $ runTDFATest $ do
        ("% No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("% So entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% So entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("% So entries found" =~ "^% No entries found" :: String) `shouldBe` ("% So entries found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("% So entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% So entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("%~No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("%~No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("%~No entries found" =~ "^% No entries found" :: String) `shouldBe` ("%~No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("%~No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("%~No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("R No entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("R No entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("R No entries found" =~ "^% No entries found" :: String) `shouldBe` ("R No entries found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("R No entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("R No entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("% NoKentries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% NoKentries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("% NoKentries found" =~ "^% No entries found" :: String) `shouldBe` ("% NoKentries found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("% NoKentries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% NoKentries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("% No entries found&" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entries found&" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("% No entries found&" =~ "^% No entries found" :: String) `shouldBe` ("% No entries found&" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("% No entries found&" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entries found&" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("% No4entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No4entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("% No4entries found" =~ "^% No entries found" :: String) `shouldBe` ("% No4entries found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("% No4entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No4entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("% xo entries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% xo entries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("% xo entries found" =~ "^% No entries found" :: String) `shouldBe` ("% xo entries found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("% xo entries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% xo entries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("% Nozentries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% Nozentries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("% Nozentries found" =~ "^% No entries found" :: String) `shouldBe` ("% Nozentries found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("% Nozentries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% Nozentries found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("% No entri(s found" =~ "^% No entries found" :: Bool) `shouldBe` ("% No entri(s found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("% No entri(s found" =~ "^% No entries found" :: String) `shouldBe` ("% No entri(s found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("% No entri(s found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% No entri(s found" TDFA.=~ "^% No entries found" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("% NoUentries found" =~ "^% No entries found" :: Bool) `shouldBe` ("% NoUentries found" TDFA.=~ "^% No entries found" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("% NoUentries found" =~ "^% No entries found" :: String) `shouldBe` ("% NoUentries found" TDFA.=~ "^% No entries found" :: String)
      it "does not match string" $ runTDFATest $ do
        ("% NoUentries found" =~ "^% No entries found" :: (String, String, String)) `shouldBe` ("% NoUentries found" TDFA.=~ "^% No entries found" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("<S?7>; rel=\"https://tent'io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<S?7>; rel=\"https://tent'io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<S?7>; rel=\"https://tent'io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<S?7>; rel=\"https://tent'io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does match string" $ runTDFATest $ do
        ("<S?7>; rel=\"https://tent'io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<S?7>; rel=\"https://tent'io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<v.{nyCJa_V>; rel=\"https://tentMio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<v.{nyCJa_V>; rel=\"https://tentMio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<v.{nyCJa_V>; rel=\"https://tentMio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<v.{nyCJa_V>; rel=\"https://tentMio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does match string" $ runTDFATest $ do
        ("<v.{nyCJa_V>; rel=\"https://tentMio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<v.{nyCJa_V>; rel=\"https://tentMio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<3ti,L5&o>; rel=\"https://tentyio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<3ti,L5&o>; rel=\"https://tentyio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<3ti,L5&o>; rel=\"https://tentyio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<3ti,L5&o>; rel=\"https://tentyio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does match string" $ runTDFATest $ do
        ("<3ti,L5&o>; rel=\"https://tentyio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<3ti,L5&o>; rel=\"https://tentyio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<Ra>; rel=\"https://tent~io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<Ra>; rel=\"https://tent~io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<Ra>; rel=\"https://tent~io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<Ra>; rel=\"https://tent~io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does match string" $ runTDFATest $ do
        ("<Ra>; rel=\"https://tent~io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<Ra>; rel=\"https://tent~io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<P_xyq4k=ek>; rel=\"https://tent6io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<P_xyq4k=ek>; rel=\"https://tent6io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<P_xyq4k=ek>; rel=\"https://tent6io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<P_xyq4k=ek>; rel=\"https://tent6io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does match string" $ runTDFATest $ do
        ("<P_xyq4k=ek>; rel=\"https://tent6io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<P_xyq4k=ek>; rel=\"https://tent6io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<tE>; rel=\"https://tent_io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<tE>; rel=\"https://tent_io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<tE>; rel=\"https://tent_io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<tE>; rel=\"https://tent_io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does match string" $ runTDFATest $ do
        ("<tE>; rel=\"https://tent_io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<tE>; rel=\"https://tent_io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<+A%XC>; rel=\"https:x/tentdio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<+A%XC>; rel=\"https:x/tentdio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<+A%XC>; rel=\"https:x/tentdio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<+A%XC>; rel=\"https:x/tentdio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<+A%XC>; rel=\"https:x/tentdio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<+A%XC>; rel=\"https:x/tentdio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<Zfisx>; rel=\"https://tent'io/rels/prof$le\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<Zfisx>; rel=\"https://tent'io/rels/prof$le\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<Zfisx>; rel=\"https://tent'io/rels/prof$le\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<Zfisx>; rel=\"https://tent'io/rels/prof$le\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<Zfisx>; rel=\"https://tent'io/rels/prof$le\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<Zfisx>; rel=\"https://tent'io/rels/prof$le\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<wc~~r.; rel=\"https://tent@io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<wc~~r.; rel=\"https://tent@io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<wc~~r.; rel=\"https://tent@io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<wc~~r.; rel=\"https://tent@io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<wc~~r.; rel=\"https://tent@io/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<wc~~r.; rel=\"https://tent@io/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<Pj0[yh>; rel=\"https://tentNio/rels/profil)\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<Pj0[yh>; rel=\"https://tentNio/rels/profil)\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<Pj0[yh>; rel=\"https://tentNio/rels/profil)\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<Pj0[yh>; rel=\"https://tentNio/rels/profil)\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<Pj0[yh>; rel=\"https://tentNio/rels/profil)\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<Pj0[yh>; rel=\"https://tentNio/rels/profil)\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<6}6p~k`>; rel=\"httNs://tentIio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<6}6p~k`>; rel=\"httNs://tentIio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<6}6p~k`>; rel=\"httNs://tentIio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<6}6p~k`>; rel=\"httNs://tentIio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<6}6p~k`>; rel=\"httNs://tentIio/rels/profile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<6}6p~k`>; rel=\"httNs://tentIio/rels/profile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<tN5w|_CQ>; rel=\"https://tentMio/relseprofile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<tN5w|_CQ>; rel=\"https://tentMio/relseprofile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<tN5w|_CQ>; rel=\"https://tentMio/relseprofile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<tN5w|_CQ>; rel=\"https://tentMio/relseprofile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<tN5w|_CQ>; rel=\"https://tentMio/relseprofile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<tN5w|_CQ>; rel=\"https://tentMio/relseprofile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<xhpEZJ3>; rel=\"https://tent.io/rels/Jrofile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool) `shouldBe` ("<xhpEZJ3>; rel=\"https://tent.io/rels/Jrofile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<xhpEZJ3>; rel=\"https://tent.io/rels/Jrofile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String) `shouldBe` ("<xhpEZJ3>; rel=\"https://tent.io/rels/Jrofile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<xhpEZJ3>; rel=\"https://tent.io/rels/Jrofile\"" =~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String)) `shouldBe` ("<xhpEZJ3>; rel=\"https://tent.io/rels/Jrofile\"" TDFA.=~ "^<(.+)>; rel=\"https://tent.io/rels/profile\"$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        (":8:55:0:7:04:27:50" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":8:55:0:7:04:27:50" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":8:55:0:7:04:27:50" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":8:55:0:7:04:27:50" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":8:55:0:7:04:27:50" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":8:55:0:7:04:27:50" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":57" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":57" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":57" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":57" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":57" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":57" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":43" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":43" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":43" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":43" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":43" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":43" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":8" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":8" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":8" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":8" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":8" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":8" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":53:51:37" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":53:51:37" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":53:51:37" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":53:51:37" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":53:51:37" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":53:51:37" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":24:52:2:5:3:1:38" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":24:52:2:5:3:1:38" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":24:52:2:5:3:1:38" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":24:52:2:5:3:1:38" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":24:52:2:5:3:1:38" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":24:52:2:5:3:1:38" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":5:8:20:9:1:1:8:53:22" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":5:8:20:9:1:1:8:53:22" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":5:8:20:9:1:1:8:53:22" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":5:8:20:9:1:1:8:53:22" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":5:8:20:9:1:1:8:53:22" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":5:8:20:9:1:1:8:53:22" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":7:59" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":7:59" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":7:59" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":7:59" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":7:59" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":7:59" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":57:31:28:19:9:58:03" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":57:31:28:19:9:58:03" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":57:31:28:19:9:58:03" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":57:31:28:19:9:58:03" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":57:31:28:19:9:58:03" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":57:31:28:19:9:58:03" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":41" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":41" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":41" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":41" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does match string" $ runTDFATest $ do
        (":41" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":41" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("209:0" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` ("209:0" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("209:0" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` ("209:0" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("209:0" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` ("209:0" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("h8:9:10" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` ("h8:9:10" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("h8:9:10" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` ("h8:9:10" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("h8:9:10" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` ("h8:9:10" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":08:3:19H00:5:29:8:3:42" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":08:3:19H00:5:29:8:3:42" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":08:3:19H00:5:29:8:3:42" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":08:3:19H00:5:29:8:3:42" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":08:3:19H00:5:29:8:3:42" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":08:3:19H00:5:29:8:3:42" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":0_:6:33:9:22:1:0:5:20:31" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":0_:6:33:9:22:1:0:5:20:31" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":0_:6:33:9:22:1:0:5:20:31" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":0_:6:33:9:22:1:0:5:20:31" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":0_:6:33:9:22:1:0:5:20:31" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":0_:6:33:9:22:1:0:5:20:31" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":7:0:(:9:33" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":7:0:(:9:33" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":7:0:(:9:33" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":7:0:(:9:33" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":7:0:(:9:33" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":7:0:(:9:33" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":9:@3:21:1:59:48:32:00" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":9:@3:21:1:59:48:32:00" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":9:@3:21:1:59:48:32:00" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":9:@3:21:1:59:48:32:00" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":9:@3:21:1:59:48:32:00" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":9:@3:21:1:59:48:32:00" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("f33" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` ("f33" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("f33" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` ("f33" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("f33" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` ("f33" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":9:23:" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":9:23:" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":9:23:" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":9:23:" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":9:23:" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":9:23:" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (":58:56:56:28:0):59:2:46" =~ "^(:[0-5]?[0-9])+$" :: Bool) `shouldBe` (":58:56:56:28:0):59:2:46" TDFA.=~ "^(:[0-5]?[0-9])+$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (":58:56:56:28:0):59:2:46" =~ "^(:[0-5]?[0-9])+$" :: String) `shouldBe` (":58:56:56:28:0):59:2:46" TDFA.=~ "^(:[0-5]?[0-9])+$" :: String)
      it "does not match string" $ runTDFATest $ do
        (":58:56:56:28:0):59:2:46" =~ "^(:[0-5]?[0-9])+$" :: (String, String, String)) `shouldBe` (":58:56:56:28:0):59:2:46" TDFA.=~ "^(:[0-5]?[0-9])+$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: Bool) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: String) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: String)
      it "does match string" $ runTDFATest $ do
        ("$a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$ax>" =~ "\\$a->" :: Bool) `shouldBe` ("$ax>" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$ax>" =~ "\\$a->" :: String) `shouldBe` ("$ax>" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$ax>" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$ax>" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Ya->" =~ "\\$a->" :: Bool) `shouldBe` ("Ya->" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Ya->" =~ "\\$a->" :: String) `shouldBe` ("Ya->" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Ya->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("Ya->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Ta->" =~ "\\$a->" :: Bool) `shouldBe` ("Ta->" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Ta->" =~ "\\$a->" :: String) `shouldBe` ("Ta->" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Ta->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("Ta->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$a-%" =~ "\\$a->" :: Bool) `shouldBe` ("$a-%" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$a-%" =~ "\\$a->" :: String) `shouldBe` ("$a-%" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$a-%" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a-%" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$a->N" =~ "\\$a->" :: Bool) `shouldBe` ("$a->N" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$a->N" =~ "\\$a->" :: String) `shouldBe` ("$a->N" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$a->N" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->N" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$am>" =~ "\\$a->" :: Bool) `shouldBe` ("$am>" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$am>" =~ "\\$a->" :: String) `shouldBe` ("$am>" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$am>" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$am>" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$a^>" =~ "\\$a->" :: Bool) `shouldBe` ("$a^>" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$a^>" =~ "\\$a->" :: String) `shouldBe` ("$a^>" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$a^>" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a^>" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a->" =~ "\\$a->" :: Bool) `shouldBe` ("<a->" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a->" =~ "\\$a->" :: String) `shouldBe` ("<a->" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("<a->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$#->" =~ "\\$a->" :: Bool) `shouldBe` ("$#->" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$#->" =~ "\\$a->" :: String) `shouldBe` ("$#->" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$#->" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$#->" TDFA.=~ "\\$a->" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$a->{" =~ "\\$a->" :: Bool) `shouldBe` ("$a->{" TDFA.=~ "\\$a->" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$a->{" =~ "\\$a->" :: String) `shouldBe` ("$a->{" TDFA.=~ "\\$a->" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$a->{" =~ "\\$a->" :: (String, String, String)) `shouldBe` ("$a->{" TDFA.=~ "\\$a->" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("__fndxai__ = \"Ovnm/t<" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__fndxai__ = \"Ovnm/t<" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("__fndxai__ = \"Ovnm/t<" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__fndxai__ = \"Ovnm/t<" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("__fndxai__ = \"Ovnm/t<" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__fndxai__ = \"Ovnm/t<" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("__zxvaoq__ = \"ZU/\\7" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__zxvaoq__ = \"ZU/\\7" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("__zxvaoq__ = \"ZU/\\7" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__zxvaoq__ = \"ZU/\\7" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("__zxvaoq__ = \"ZU/\\7" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__zxvaoq__ = \"ZU/\\7" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("__wsef__ = \"<?Sw_l$0" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__wsef__ = \"<?Sw_l$0" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("__wsef__ = \"<?Sw_l$0" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__wsef__ = \"<?Sw_l$0" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("__wsef__ = \"<?Sw_l$0" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__wsef__ = \"<?Sw_l$0" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("__gfpovmin__ = \"u#tQ-dgae" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__gfpovmin__ = \"u#tQ-dgae" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("__gfpovmin__ = \"u#tQ-dgae" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__gfpovmin__ = \"u#tQ-dgae" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("__gfpovmin__ = \"u#tQ-dgae" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__gfpovmin__ = \"u#tQ-dgae" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("__ok__ = \"}UK[2Z@}t" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__ok__ = \"}UK[2Z@}t" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("__ok__ = \"}UK[2Z@}t" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__ok__ = \"}UK[2Z@}t" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("__ok__ = \"}UK[2Z@}t" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__ok__ = \"}UK[2Z@}t" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("__bylnp__ = \":7 B9y" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__bylnp__ = \":7 B9y" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("__bylnp__ = \":7 B9y" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__bylnp__ = \":7 B9y" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("__bylnp__ = \":7 B9y" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__bylnp__ = \":7 B9y" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("__po__ = \"XZvcd" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__po__ = \"XZvcd" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("__po__ = \"XZvcd" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__po__ = \"XZvcd" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("__po__ = \"XZvcd" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__po__ = \"XZvcd" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("__zcy__ = \"<KdKeG" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__zcy__ = \"<KdKeG" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("__zcy__ = \"<KdKeG" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__zcy__ = \"<KdKeG" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("__zcy__ = \"<KdKeG" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__zcy__ = \"<KdKeG" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("__lljvsunpes__ = ]]" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__lljvsunpes__ = ]]" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("__lljvsunpes__ = ]]" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__lljvsunpes__ = ]]" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("__lljvsunpes__ = ]]" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__lljvsunpes__ = ]]" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("__vwVzra__ = \")" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__vwVzra__ = \")" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("__vwVzra__ = \")" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__vwVzra__ = \")" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("__vwVzra__ = \")" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__vwVzra__ = \")" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("__qnylbc__ =_\"*PbVIM{t8q" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__qnylbc__ =_\"*PbVIM{t8q" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("__qnylbc__ =_\"*PbVIM{t8q" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__qnylbc__ =_\"*PbVIM{t8q" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("__qnylbc__ =_\"*PbVIM{t8q" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__qnylbc__ =_\"*PbVIM{t8q" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("__iivkditxz__x= \"H5\\/|" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__iivkditxz__x= \"H5\\/|" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("__iivkditxz__x= \"H5\\/|" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__iivkditxz__x= \"H5\\/|" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("__iivkditxz__x= \"H5\\/|" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__iivkditxz__x= \"H5\\/|" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("__ukrqhg%a__ = \"i?:Cr<lB" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__ukrqhg%a__ = \"i?:Cr<lB" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("__ukrqhg%a__ = \"i?:Cr<lB" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__ukrqhg%a__ = \"i?:Cr<lB" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("__ukrqhg%a__ = \"i?:Cr<lB" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__ukrqhg%a__ = \"i?:Cr<lB" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("__ofebug,vn__ = \"QUkM.T>" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__ofebug,vn__ = \"QUkM.T>" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("__ofebug,vn__ = \"QUkM.T>" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__ofebug,vn__ = \"QUkM.T>" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("__ofebug,vn__ = \"QUkM.T>" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__ofebug,vn__ = \"QUkM.T>" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("__>__ = \"7!Q(o" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("__>__ = \"7!Q(o" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("__>__ = \"7!Q(o" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("__>__ = \"7!Q(o" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("__>__ = \"7!Q(o" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("__>__ = \"7!Q(o" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("-_ilpz__ = \"S(8O<C9F&" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("-_ilpz__ = \"S(8O<C9F&" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("-_ilpz__ = \"S(8O<C9F&" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("-_ilpz__ = \"S(8O<C9F&" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("-_ilpz__ = \"S(8O<C9F&" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("-_ilpz__ = \"S(8O<C9F&" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("_Gf__ = \"(@o" =~ "__([a-z]+)__ = \"([^\"]+)" :: Bool) `shouldBe` ("_Gf__ = \"(@o" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("_Gf__ = \"(@o" =~ "__([a-z]+)__ = \"([^\"]+)" :: String) `shouldBe` ("_Gf__ = \"(@o" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("_Gf__ = \"(@o" =~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String)) `shouldBe` ("_Gf__ = \"(@o" TDFA.=~ "__([a-z]+)__ = \"([^\"]+)" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("S" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("S" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("S" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("S" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("S" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("S" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("S" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("S" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("S" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("S" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("S" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("S" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("W" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("W" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("W" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("W" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("W" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("W" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("R" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("R" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("R" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("R" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("R" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("R" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("M" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("M" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("K" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("K" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("K" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("K" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("K" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("K" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("R" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("R" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("R" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("R" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does match string" $ runTDFATest $ do
        ("R" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("R" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("5" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("5" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("5" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("5" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        ("5" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("5" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Kt" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("Kt" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Kt" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("Kt" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Kt" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("Kt" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("9" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("9" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("9" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("9" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        ("9" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("9" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Rn" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("Rn" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Rn" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("Rn" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Rn" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("Rn" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("n" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("n" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("n" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("n" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        ("n" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("n" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("KT" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("KT" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("KT" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("KT" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        ("KT" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("KT" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("M^" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("M^" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("M^" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("M^" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        ("M^" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("M^" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (">" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` (">" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (">" =~ "K|Y|W|M|R|S" :: String) `shouldBe` (">" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        (">" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` (">" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("|" =~ "K|Y|W|M|R|S" :: Bool) `shouldBe` ("|" TDFA.=~ "K|Y|W|M|R|S" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("|" =~ "K|Y|W|M|R|S" :: String) `shouldBe` ("|" TDFA.=~ "K|Y|W|M|R|S" :: String)
      it "does not match string" $ runTDFATest $ do
        ("|" =~ "K|Y|W|M|R|S" :: (String, String, String)) `shouldBe` ("|" TDFA.=~ "K|Y|W|M|R|S" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("9:return:6*U:+" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("9:return:6*U:+" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("9:return:6*U:+" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("9:return:6*U:+" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does match string" $ runTDFATest $ do
        ("9:return:6*U:+" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("9:return:6*U:+" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("1:return:4" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("1:return:4" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("1:return:4" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("1:return:4" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does match string" $ runTDFATest $ do
        ("1:return:4" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("1:return:4" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("99:return:|2Fv/Kb" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("99:return:|2Fv/Kb" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("99:return:|2Fv/Kb" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("99:return:|2Fv/Kb" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does match string" $ runTDFATest $ do
        ("99:return:|2Fv/Kb" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("99:return:|2Fv/Kb" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("63970721:return:" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("63970721:return:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("63970721:return:" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("63970721:return:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does match string" $ runTDFATest $ do
        ("63970721:return:" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("63970721:return:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("59:return:]l" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("59:return:]l" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("59:return:]l" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("59:return:]l" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does match string" $ runTDFATest $ do
        ("59:return:]l" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("59:return:]l" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("739:return:" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("739:return:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("739:return:" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("739:return:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does match string" $ runTDFATest $ do
        ("739:return:" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("739:return:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("2099862:return:D y#" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("2099862:return:D y#" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("2099862:return:D y#" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("2099862:return:D y#" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does match string" $ runTDFATest $ do
        ("2099862:return:D y#" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("2099862:return:D y#" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("44:return:Xn<-" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("44:return:Xn<-" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("44:return:Xn<-" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("44:return:Xn<-" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does match string" $ runTDFATest $ do
        ("44:return:Xn<-" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("44:return:Xn<-" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("4545a5306:return:8|*" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("4545a5306:return:8|*" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("4545a5306:return:8|*" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("4545a5306:return:8|*" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("4545a5306:return:8|*" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("4545a5306:return:8|*" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("2763%12:return:}" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("2763%12:return:}" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("2763%12:return:}" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("2763%12:return:}" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("2763%12:return:}" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("2763%12:return:}" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("6646I:return:q:GQK:" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("6646I:return:q:GQK:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("6646I:return:q:GQK:" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("6646I:return:q:GQK:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("6646I:return:q:GQK:" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("6646I:return:q:GQK:" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("091920251:retBrn:mh|" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("091920251:retBrn:mh|" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("091920251:retBrn:mh|" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("091920251:retBrn:mh|" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("091920251:retBrn:mh|" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("091920251:retBrn:mh|" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("67786:rxturn:W@zzh}K5@" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("67786:rxturn:W@zzh}K5@" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("67786:rxturn:W@zzh}K5@" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("67786:rxturn:W@zzh}K5@" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("67786:rxturn:W@zzh}K5@" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("67786:rxturn:W@zzh}K5@" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("05h1432:return:8$cBHrPcw\\" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("05h1432:return:8$cBHrPcw\\" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("05h1432:return:8$cBHrPcw\\" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("05h1432:return:8$cBHrPcw\\" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("05h1432:return:8$cBHrPcw\\" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("05h1432:return:8$cBHrPcw\\" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("0:reDurn:R" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("0:reDurn:R" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("0:reDurn:R" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("0:reDurn:R" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("0:reDurn:R" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("0:reDurn:R" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("08102882y:return:U#A+\"@^a" =~ "^\\d+:return:(.*)" :: Bool) `shouldBe` ("08102882y:return:U#A+\"@^a" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("08102882y:return:U#A+\"@^a" =~ "^\\d+:return:(.*)" :: String) `shouldBe` ("08102882y:return:U#A+\"@^a" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("08102882y:return:U#A+\"@^a" =~ "^\\d+:return:(.*)" :: (String, String, String)) `shouldBe` ("08102882y:return:U#A+\"@^a" TDFA.=~ "^[[:digit:]]+:return:(.*)" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("2t4biW" =~ "(\\w+)" :: Bool) `shouldBe` ("2t4biW" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("2t4biW" =~ "(\\w+)" :: String) `shouldBe` ("2t4biW" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("2t4biW" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("2t4biW" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("JDhn7V" =~ "(\\w+)" :: Bool) `shouldBe` ("JDhn7V" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("JDhn7V" =~ "(\\w+)" :: String) `shouldBe` ("JDhn7V" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("JDhn7V" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("JDhn7V" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("9fdtTlzu4U" =~ "(\\w+)" :: Bool) `shouldBe` ("9fdtTlzu4U" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("9fdtTlzu4U" =~ "(\\w+)" :: String) `shouldBe` ("9fdtTlzu4U" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("9fdtTlzu4U" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("9fdtTlzu4U" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("s1yJyrgq8" =~ "(\\w+)" :: Bool) `shouldBe` ("s1yJyrgq8" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("s1yJyrgq8" =~ "(\\w+)" :: String) `shouldBe` ("s1yJyrgq8" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("s1yJyrgq8" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("s1yJyrgq8" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("P" =~ "(\\w+)" :: Bool) `shouldBe` ("P" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("P" =~ "(\\w+)" :: String) `shouldBe` ("P" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("P" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("P" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("NPx4" =~ "(\\w+)" :: Bool) `shouldBe` ("NPx4" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("NPx4" =~ "(\\w+)" :: String) `shouldBe` ("NPx4" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("NPx4" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("NPx4" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Go6ys" =~ "(\\w+)" :: Bool) `shouldBe` ("Go6ys" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Go6ys" =~ "(\\w+)" :: String) `shouldBe` ("Go6ys" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Go6ys" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("Go6ys" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("JAjTp" =~ "(\\w+)" :: Bool) `shouldBe` ("JAjTp" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("JAjTp" =~ "(\\w+)" :: String) `shouldBe` ("JAjTp" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("JAjTp" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("JAjTp" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Amp" =~ "(\\w+)" :: Bool) `shouldBe` ("Amp" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Amp" =~ "(\\w+)" :: String) `shouldBe` ("Amp" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("Amp" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("Amp" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("yQ" =~ "(\\w+)" :: Bool) `shouldBe` ("yQ" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("yQ" =~ "(\\w+)" :: String) `shouldBe` ("yQ" TDFA.=~ "([[:word:]]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("yQ" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("yQ" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("d:4Q" =~ "(\\w+)" :: Bool) `shouldBe` ("d:4Q" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("d:4Q" =~ "(\\w+)" :: String) `shouldBe` ("d:4Q" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("d:4Q" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("d:4Q" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("l*" =~ "(\\w+)" :: Bool) `shouldBe` ("l*" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("l*" =~ "(\\w+)" :: String) `shouldBe` ("l*" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("l*" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("l*" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("By-i" =~ "(\\w+)" :: Bool) `shouldBe` ("By-i" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("By-i" =~ "(\\w+)" :: String) `shouldBe` ("By-i" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("By-i" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("By-i" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("J#Qo7kb" =~ "(\\w+)" :: Bool) `shouldBe` ("J#Qo7kb" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("J#Qo7kb" =~ "(\\w+)" :: String) `shouldBe` ("J#Qo7kb" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("J#Qo7kb" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("J#Qo7kb" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("fMZW\\" =~ "(\\w+)" :: Bool) `shouldBe` ("fMZW\\" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("fMZW\\" =~ "(\\w+)" :: String) `shouldBe` ("fMZW\\" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("fMZW\\" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("fMZW\\" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("yU\\1" =~ "(\\w+)" :: Bool) `shouldBe` ("yU\\1" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("yU\\1" =~ "(\\w+)" :: String) `shouldBe` ("yU\\1" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("yU\\1" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("yU\\1" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("QNnMt=QWV" =~ "(\\w+)" :: Bool) `shouldBe` ("QNnMt=QWV" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("QNnMt=QWV" =~ "(\\w+)" :: String) `shouldBe` ("QNnMt=QWV" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("QNnMt=QWV" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("QNnMt=QWV" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mAdnr05'P" =~ "(\\w+)" :: Bool) `shouldBe` ("mAdnr05'P" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mAdnr05'P" =~ "(\\w+)" :: String) `shouldBe` ("mAdnr05'P" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mAdnr05'P" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("mAdnr05'P" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("~" =~ "(\\w+)" :: Bool) `shouldBe` ("~" TDFA.=~ "([[:word:]]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("~" =~ "(\\w+)" :: String) `shouldBe` ("~" TDFA.=~ "([[:word:]]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("~" =~ "(\\w+)" :: (String, String, String)) `shouldBe` ("~" TDFA.=~ "([[:word:]]+)" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does match string" $ runTDFATest $ do
        ("as anonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as anonym'us user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonym'us user" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as anonym'us user" =~ "as anonymous user" :: String) `shouldBe` ("as anonym'us user" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as anonym'us user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonym'us user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as anonymouo user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymouo user" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as anonymouo user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymouo user" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as anonymouo user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymouo user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as anonypous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonypous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as anonypous user" =~ "as anonymous user" :: String) `shouldBe` ("as anonypous user" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as anonypous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonypous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as anonymous uJer" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous uJer" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as anonymous uJer" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous uJer" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as anonymous uJer" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous uJer" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as .nonymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as .nonymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as .nonymous user" =~ "as anonymous user" :: String) `shouldBe` ("as .nonymous user" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as .nonymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as .nonymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as anonymZus user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymZus user" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as anonymZus user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymZus user" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as anonymZus user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymZus user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as ano=ymous user" =~ "as anonymous user" :: Bool) `shouldBe` ("as ano=ymous user" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as ano=ymous user" =~ "as anonymous user" :: String) `shouldBe` ("as ano=ymous user" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as ano=ymous user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as ano=ymous user" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as anonymous userm" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymous userm" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as anonymous userm" =~ "as anonymous user" :: String) `shouldBe` ("as anonymous userm" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as anonymous userm" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymous userm" TDFA.=~ "as anonymous user" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("as anonymLus user" =~ "as anonymous user" :: Bool) `shouldBe` ("as anonymLus user" TDFA.=~ "as anonymous user" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("as anonymLus user" =~ "as anonymous user" :: String) `shouldBe` ("as anonymLus user" TDFA.=~ "as anonymous user" :: String)
      it "does not match string" $ runTDFATest $ do
        ("as anonymLus user" =~ "as anonymous user" :: (String, String, String)) `shouldBe` ("as anonymLus user" TDFA.=~ "as anonymous user" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        (" ," =~ "[,\\. ]+" :: Bool) `shouldBe` (" ," TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        (" ," =~ "[,\\. ]+" :: String) `shouldBe` (" ," TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        (" ," =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (" ," TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("..," =~ "[,\\. ]+" :: Bool) `shouldBe` ("..," TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("..," =~ "[,\\. ]+" :: String) `shouldBe` ("..," TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("..," =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("..," TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("..,, , ,." =~ "[,\\. ]+" :: Bool) `shouldBe` ("..,, , ,." TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("..,, , ,." =~ "[,\\. ]+" :: String) `shouldBe` ("..,, , ,." TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("..,, , ,." =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("..,, , ,." TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (", ..,,." =~ "[,\\. ]+" :: Bool) `shouldBe` (", ..,,." TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        (", ..,,." =~ "[,\\. ]+" :: String) `shouldBe` (", ..,,." TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        (", ..,,." =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (", ..,,." TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (" , ." =~ "[,\\. ]+" :: Bool) `shouldBe` (" , ." TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        (" , ." =~ "[,\\. ]+" :: String) `shouldBe` (" , ." TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        (" , ." =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (" , ." TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (".,, . " =~ "[,\\. ]+" :: Bool) `shouldBe` (".,, . " TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        (".,, . " =~ "[,\\. ]+" :: String) `shouldBe` (".,, . " TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        (".,, . " =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (".,, . " TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",," =~ "[,\\. ]+" :: Bool) `shouldBe` (",," TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",," =~ "[,\\. ]+" :: String) `shouldBe` (",," TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        (",," =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (",," TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",.,   " =~ "[,\\. ]+" :: Bool) `shouldBe` (",.,   " TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",.,   " =~ "[,\\. ]+" :: String) `shouldBe` (",.,   " TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        (",.,   " =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (",.,   " TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (",, " =~ "[,\\. ]+" :: Bool) `shouldBe` (",, " TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        (",, " =~ "[,\\. ]+" :: String) `shouldBe` (",, " TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        (",, " =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (",, " TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("....,..," =~ "[,\\. ]+" :: Bool) `shouldBe` ("....,..," TDFA.=~ "[,\\. ]+" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("....,..," =~ "[,\\. ]+" :: String) `shouldBe` ("....,..," TDFA.=~ "[,\\. ]+" :: String)
      it "does match string" $ runTDFATest $ do
        ("....,..," =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("....,..," TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("M  " =~ "[,\\. ]+" :: Bool) `shouldBe` ("M  " TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("M  " =~ "[,\\. ]+" :: String) `shouldBe` ("M  " TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("M  " =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("M  " TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("..  ,, ,.5" =~ "[,\\. ]+" :: Bool) `shouldBe` ("..  ,, ,.5" TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("..  ,, ,.5" =~ "[,\\. ]+" :: String) `shouldBe` ("..  ,, ,.5" TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("..  ,, ,.5" =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("..  ,, ,.5" TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("2. ,. ,, ." =~ "[,\\. ]+" :: Bool) `shouldBe` ("2. ,. ,, ." TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("2. ,. ,, ." =~ "[,\\. ]+" :: String) `shouldBe` ("2. ,. ,, ." TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("2. ,. ,, ." =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("2. ,. ,, ." TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (" U" =~ "[,\\. ]+" :: Bool) `shouldBe` (" U" TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (" U" =~ "[,\\. ]+" :: String) `shouldBe` (" U" TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        (" U" =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (" U" TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (".,,C" =~ "[,\\. ]+" :: Bool) `shouldBe` (".,,C" TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (".,,C" =~ "[,\\. ]+" :: String) `shouldBe` (".,,C" TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        (".,,C" =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (".,,C" TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (",#" =~ "[,\\. ]+" :: Bool) `shouldBe` (",#" TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (",#" =~ "[,\\. ]+" :: String) `shouldBe` (",#" TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        (",#" =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (",#" TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("P" =~ "[,\\. ]+" :: Bool) `shouldBe` ("P" TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("P" =~ "[,\\. ]+" :: String) `shouldBe` ("P" TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("P" =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("P" TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("A" =~ "[,\\. ]+" :: Bool) `shouldBe` ("A" TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("A" =~ "[,\\. ]+" :: String) `shouldBe` ("A" TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("A" =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("A" TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("K  .,.,,, " =~ "[,\\. ]+" :: Bool) `shouldBe` ("K  .,.,,, " TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("K  .,.,,, " =~ "[,\\. ]+" :: String) `shouldBe` ("K  .,.,,, " TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        ("K  .,.,,, " =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` ("K  .,.,,, " TDFA.=~ "[,\\. ]+" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (" t,,,, " =~ "[,\\. ]+" :: Bool) `shouldBe` (" t,,,, " TDFA.=~ "[,\\. ]+" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (" t,,,, " =~ "[,\\. ]+" :: String) `shouldBe` (" t,,,, " TDFA.=~ "[,\\. ]+" :: String)
      it "does not match string" $ runTDFATest $ do
        (" t,,,, " =~ "[,\\. ]+" :: (String, String, String)) `shouldBe` (" t,,,, " TDFA.=~ "[,\\. ]+" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("create*^db/migrate/20_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("create*^db/migrate/20_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("create*^db/migrate/20_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("create*^db/migrate/20_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("create*^db/migrate/20_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("create*^db/migrate/20_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/2215_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createdb/migrate/2215_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/2215_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createdb/migrate/2215_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/2215_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createdb/migrate/2215_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("createS:xdb/migrate/850091_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createS:xdb/migrate/850091_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("createS:xdb/migrate/850091_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createS:xdb/migrate/850091_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("createS:xdb/migrate/850091_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createS:xdb/migrate/850091_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("createsYWOWVzo\"db/migrate/79479579_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createsYWOWVzo\"db/migrate/79479579_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("createsYWOWVzo\"db/migrate/79479579_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createsYWOWVzo\"db/migrate/79479579_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("createsYWOWVzo\"db/migrate/79479579_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createsYWOWVzo\"db/migrate/79479579_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("create5.E#`db/migrate/551_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("create5.E#`db/migrate/551_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("create5.E#`db/migrate/551_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("create5.E#`db/migrate/551_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("create5.E#`db/migrate/551_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("create5.E#`db/migrate/551_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/5492943117_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createdb/migrate/5492943117_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/5492943117_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createdb/migrate/5492943117_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/5492943117_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createdb/migrate/5492943117_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("createTtbiSK'ndb/migrate/279420809_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createTtbiSK'ndb/migrate/279420809_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("createTtbiSK'ndb/migrate/279420809_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createTtbiSK'ndb/migrate/279420809_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("createTtbiSK'ndb/migrate/279420809_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createTtbiSK'ndb/migrate/279420809_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("createj_V:c\\M$db/migrate/4628858_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createj_V:c\\M$db/migrate/4628858_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("createj_V:c\\M$db/migrate/4628858_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createj_V:c\\M$db/migrate/4628858_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("createj_V:c\\M$db/migrate/4628858_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createj_V:c\\M$db/migrate/4628858_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("create6SKL=m!+-db/migrate/73_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("create6SKL=m!+-db/migrate/73_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("create6SKL=m!+-db/migrate/73_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("create6SKL=m!+-db/migrate/73_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("create6SKL=m!+-db/migrate/73_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("create6SKL=m!+-db/migrate/73_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/065031208_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createdb/migrate/065031208_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/065031208_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createdb/migrate/065031208_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does match string" $ runTDFATest $ do
        ("createdb/migrate/065031208_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createdb/migrate/065031208_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("createQ}_hjd;/migrate/235607115_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createQ}_hjd;/migrate/235607115_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("createQ}_hjd;/migrate/235607115_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createQ}_hjd;/migrate/235607115_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does not match string" $ runTDFATest $ do
        ("createQ}_hjd;/migrate/235607115_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createQ}_hjd;/migrate/235607115_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("create<ldb/migrate/01038_tes_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("create<ldb/migrate/01038_tes_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("create<ldb/migrate/01038_tes_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("create<ldb/migrate/01038_tes_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does not match string" $ runTDFATest $ do
        ("create<ldb/migrate/01038_tes_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("create<ldb/migrate/01038_tes_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("createhaK{!XeKh$db/migrIte/6_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createhaK{!XeKh$db/migrIte/6_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("createhaK{!XeKh$db/migrIte/6_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createhaK{!XeKh$db/migrIte/6_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does not match string" $ runTDFATest $ do
        ("createhaK{!XeKh$db/migrIte/6_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createhaK{!XeKh$db/migrIte/6_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("createwZ;Pl\\fZb/migrate/8708610_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createwZ;Pl\\fZb/migrate/8708610_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("createwZ;Pl\\fZb/migrate/8708610_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createwZ;Pl\\fZb/migrate/8708610_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does not match string" $ runTDFATest $ do
        ("createwZ;Pl\\fZb/migrate/8708610_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createwZ;Pl\\fZb/migrate/8708610_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("createdb/migrate/6462209835_te*t_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createdb/migrate/6462209835_te*t_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("createdb/migrate/6462209835_te*t_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createdb/migrate/6462209835_te*t_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does not match string" $ runTDFATest $ do
        ("createdb/migrate/6462209835_te*t_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createdb/migrate/6462209835_te*t_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("createxzBwdb/migpate/901220_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("createxzBwdb/migpate/901220_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("createxzBwdb/migpate/901220_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("createxzBwdb/migpate/901220_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does not match string" $ runTDFATest $ do
        ("createxzBwdb/migpate/901220_test_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("createxzBwdb/migpate/901220_test_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("create+9obXdb/migrate/2_sest_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("create+9obXdb/migrate/2_sest_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("create+9obXdb/migrate/2_sest_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("create+9obXdb/migrate/2_sest_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does not match string" $ runTDFATest $ do
        ("create+9obXdb/migrate/2_sest_abc_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("create+9obXdb/migrate/2_sest_abc_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("create0.@fL(Odb/migrate/48957_test_a|c_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: Bool) `shouldBe` ("create0.@fL(Odb/migrate/48957_test_a|c_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("create0.@fL(Odb/migrate/48957_test_a|c_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: String) `shouldBe` ("create0.@fL(Odb/migrate/48957_test_a|c_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: String)
      it "does not match string" $ runTDFATest $ do
        ("create0.@fL(Odb/migrate/48957_test_a|c_args.rb" =~ "create(.*)db/migrate/\\d+_test_abc_args\\.rb" :: (String, String, String)) `shouldBe` ("create0.@fL(Odb/migrate/48957_test_a|c_args.rb" TDFA.=~ "create(.*)db/migrate/[[:digit:]]+_test_abc_args\\.rb" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: String) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: String) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: String) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: String) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: String) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: String) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: String) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: String) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mesi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mesi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: String) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: String) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: String)
      it "does match string" $ runTDFATest $ do
        ("mese" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mese" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mUsi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mUsi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mUsi" =~ "^mes[ei]$" :: String) `shouldBe` ("mUsi" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mUsi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mUsi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mesiw" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mesiw" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mesiw" =~ "^mes[ei]$" :: String) `shouldBe` ("mesiw" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mesiw" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mesiw" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mesib" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mesib" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mesib" =~ "^mes[ei]$" :: String) `shouldBe` ("mesib" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mesib" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mesib" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("meHi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("meHi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("meHi" =~ "^mes[ei]$" :: String) `shouldBe` ("meHi" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("meHi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("meHi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("\"esi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("\"esi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("\"esi" =~ "^mes[ei]$" :: String) `shouldBe` ("\"esi" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("\"esi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("\"esi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Tese" =~ "^mes[ei]$" :: Bool) `shouldBe` ("Tese" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Tese" =~ "^mes[ei]$" :: String) `shouldBe` ("Tese" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Tese" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("Tese" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Lesi" =~ "^mes[ei]$" :: Bool) `shouldBe` ("Lesi" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Lesi" =~ "^mes[ei]$" :: String) `shouldBe` ("Lesi" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Lesi" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("Lesi" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mes|" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mes|" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mes|" =~ "^mes[ei]$" :: String) `shouldBe` ("mes|" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mes|" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mes|" TDFA.=~ "^mes[ei]$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("mes~" =~ "^mes[ei]$" :: Bool) `shouldBe` ("mes~" TDFA.=~ "^mes[ei]$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("mes~" =~ "^mes[ei]$" :: String) `shouldBe` ("mes~" TDFA.=~ "^mes[ei]$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("mes~" =~ "^mes[ei]$" :: (String, String, String)) `shouldBe` ("mes~" TDFA.=~ "^mes[ei]$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("!" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("!" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("!" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("!" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        ("!" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("!" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        (">" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` (">" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("?" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("?" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("?" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("?" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        ("?" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("?" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("$" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("$" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("$" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        (":" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` (":" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        (":" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` (":" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        (":" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` (":" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("[" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("[" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        ("[" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("[" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("&" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("&" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("&" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("&" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        ("&" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("&" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("(" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("(" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("(" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("(" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does match string" $ runTDFATest $ do
        ("(" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("(" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("#S" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("#S" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("#S" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("#S" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("#S" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("#S" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("%g" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("%g" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("%g" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("%g" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("%g" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("%g" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (",W" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` (",W" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (",W" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` (",W" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        (",W" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` (",W" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("1" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("1" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("1" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("1" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("1" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("1" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("f" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("f" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("f" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("8" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("8" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("8" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("8" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("8" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("8" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (",x" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` (",x" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (",x" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` (",x" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        (",x" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` (",x" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (";l" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` (";l" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (";l" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` (";l" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        (";l" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` (";l" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("V" =~ "[^-A-Za-z0-9_.]" :: Bool) `shouldBe` ("V" TDFA.=~ "[^-A-Za-z0-9_.]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("V" =~ "[^-A-Za-z0-9_.]" :: String) `shouldBe` ("V" TDFA.=~ "[^-A-Za-z0-9_.]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("V" =~ "[^-A-Za-z0-9_.]" :: (String, String, String)) `shouldBe` ("V" TDFA.=~ "[^-A-Za-z0-9_.]" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: Bool) `shouldBe` ("deny" TDFA.=~ "^deny$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: String) `shouldBe` ("deny" TDFA.=~ "^deny$" :: String)
      it "does match string" $ runTDFATest $ do
        ("deny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("de,y" =~ "^deny$" :: Bool) `shouldBe` ("de,y" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("de,y" =~ "^deny$" :: String) `shouldBe` ("de,y" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("de,y" =~ "^deny$" :: (String, String, String)) `shouldBe` ("de,y" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Teny" =~ "^deny$" :: Bool) `shouldBe` ("Teny" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Teny" =~ "^deny$" :: String) `shouldBe` ("Teny" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Teny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("Teny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("den+" =~ "^deny$" :: Bool) `shouldBe` ("den+" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("den+" =~ "^deny$" :: String) `shouldBe` ("den+" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("den+" =~ "^deny$" :: (String, String, String)) `shouldBe` ("den+" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("deny/" =~ "^deny$" :: Bool) `shouldBe` ("deny/" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("deny/" =~ "^deny$" :: String) `shouldBe` ("deny/" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("deny/" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny/" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("\"eny" =~ "^deny$" :: Bool) `shouldBe` ("\"eny" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("\"eny" =~ "^deny$" :: String) `shouldBe` ("\"eny" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("\"eny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("\"eny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Eeny" =~ "^deny$" :: Bool) `shouldBe` ("Eeny" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Eeny" =~ "^deny$" :: String) `shouldBe` ("Eeny" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Eeny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("Eeny" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("den{" =~ "^deny$" :: Bool) `shouldBe` ("den{" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("den{" =~ "^deny$" :: String) `shouldBe` ("den{" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("den{" =~ "^deny$" :: (String, String, String)) `shouldBe` ("den{" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("denL" =~ "^deny$" :: Bool) `shouldBe` ("denL" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("denL" =~ "^deny$" :: String) `shouldBe` ("denL" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("denL" =~ "^deny$" :: (String, String, String)) `shouldBe` ("denL" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("deny)" =~ "^deny$" :: Bool) `shouldBe` ("deny)" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("deny)" =~ "^deny$" :: String) `shouldBe` ("deny)" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("deny)" =~ "^deny$" :: (String, String, String)) `shouldBe` ("deny)" TDFA.=~ "^deny$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("!eny" =~ "^deny$" :: Bool) `shouldBe` ("!eny" TDFA.=~ "^deny$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("!eny" =~ "^deny$" :: String) `shouldBe` ("!eny" TDFA.=~ "^deny$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("!eny" =~ "^deny$" :: (String, String, String)) `shouldBe` ("!eny" TDFA.=~ "^deny$" :: (String, String, String))
    describe "Full matching" $ do
      it "does not match string" $ runTDFATest $ do
        ("]" =~ "[\\n\\r\\u0004]*$" :: Bool) `shouldBe` ("]" TDFA.=~ "[\\n\\r\\u0004]*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("]" =~ "[\\n\\r\\u0004]*$" :: String) `shouldBe` ("]" TDFA.=~ "[\\n\\r\\u0004]*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("]" =~ "[\\n\\r\\u0004]*$" :: (String, String, String)) `shouldBe` ("]" TDFA.=~ "[\\n\\r\\u0004]*$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: String)
      it "does match string" $ runTDFATest $ do
        ("Access is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Access is dOnied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is dOnied" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Access is dOnied" =~ "^Access is denied" :: String) `shouldBe` ("Access is dOnied" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Access is dOnied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is dOnied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Access isdenied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access isdenied" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Access isdenied" =~ "^Access is denied" :: String) `shouldBe` ("Access isdenied" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Access isdenied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access isdenied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("jccess is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("jccess is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("jccess is denied" =~ "^Access is denied" :: String) `shouldBe` ("jccess is denied" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("jccess is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("jccess is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Access is enied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is enied" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Access is enied" =~ "^Access is denied" :: String) `shouldBe` ("Access is enied" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Access is enied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is enied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Access is denied%" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denied%" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Access is denied%" =~ "^Access is denied" :: String) `shouldBe` ("Access is denied%" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Access is denied%" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denied%" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Access is6denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is6denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Access is6denied" =~ "^Access is denied" :: String) `shouldBe` ("Access is6denied" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Access is6denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is6denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Access is denidd" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is denidd" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Access is denidd" =~ "^Access is denied" :: String) `shouldBe` ("Access is denidd" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Access is denidd" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is denidd" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Access is ienied" =~ "^Access is denied" :: Bool) `shouldBe` ("Access is ienied" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Access is ienied" =~ "^Access is denied" :: String) `shouldBe` ("Access is ienied" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Access is ienied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Access is ienied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Acces\" is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Acces\" is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Acces\" is denied" =~ "^Access is denied" :: String) `shouldBe` ("Acces\" is denied" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Acces\" is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Acces\" is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Acwess is denied" =~ "^Access is denied" :: Bool) `shouldBe` ("Acwess is denied" TDFA.=~ "^Access is denied" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Acwess is denied" =~ "^Access is denied" :: String) `shouldBe` ("Acwess is denied" TDFA.=~ "^Access is denied" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Acwess is denied" =~ "^Access is denied" :: (String, String, String)) `shouldBe` ("Acwess is denied" TDFA.=~ "^Access is denied" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("tmmmmmmmorrrrrrrrrrw" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmmmmmmmorrrrrrrrrrw" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tmmmmmmmorrrrrrrrrrw" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmmmmmmmorrrrrrrrrrw" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tmmmmmmmorrrrrrrrrrw" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmmmmmmmorrrrrrrrrrw" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tommrrrrrrrw" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tommrrrrrrrw" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tommrrrrrrrw" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tommrrrrrrrw" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tommrrrrrrrw" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tommrrrrrrrw" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmmorrrrrrro" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tommmmmmmmmorrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmmorrrrrrro" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tommmmmmmmmorrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmmorrrrrrro" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tommmmmmmmmorrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tmorrrrrro" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmorrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tmorrrrrro" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmorrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tmorrrrrro" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmorrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmmrrrr" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tommmmmmmmmrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmmrrrr" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tommmmmmmmmrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmmrrrr" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tommmmmmmmmrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tomorrrrrrrrr" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tomorrrrrrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tomorrrrrrrrr" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tomorrrrrrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tomorrrrrrrrr" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tomorrrrrrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmorrrrrrro" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tommmmmmmmorrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmorrrrrrro" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tommmmmmmmorrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tommmmmmmmorrrrrrro" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tommmmmmmmorrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tmoro" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmoro" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tmoro" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmoro" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tmoro" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmoro" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tmorrrrrrrro" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmorrrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tmorrrrrrrro" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmorrrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tmorrrrrrrro" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmorrrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("tommrrrrrrrrr" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tommrrrrrrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("tommrrrrrrrrr" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tommrrrrrrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does match string" $ runTDFATest $ do
        ("tommrrrrrrrrr" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tommrrrrrrrrr" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tmmmmmmmmmoVrrow" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmmmmmmmmmoVrrow" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmmmmmmoVrrow" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmmmmmmmmmoVrrow" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmmmmmmoVrrow" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmmmmmmmmmoVrrow" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tmmmmorrrrBo" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmmmmorrrrBo" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmorrrrBo" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmmmmorrrrBo" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmorrrrBo" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmmmmorrrrBo" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tmmmmm<mmmmrrw" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmmmmm<mmmmrrw" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmm<mmmmrrw" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmmmmm<mmmmrrw" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmm<mmmmrrw" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmmmmm<mmmmrrw" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tomm^mmmmmmmorrrro" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tomm^mmmmmmmorrrro" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tomm^mmmmmmmorrrro" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tomm^mmmmmmmorrrro" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tomm^mmmmmmmorrrro" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tomm^mmmmmmmorrrro" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tmmmmrt" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmmmmrt" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmrt" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmmmmrt" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmrt" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmmmmrt" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("t1mmmmmmmmmmorow" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("t1mmmmmmmmmmorow" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("t1mmmmmmmmmmorow" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("t1mmmmmmmmmmorow" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("t1mmmmmmmmmmorow" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("t1mmmmmmmmmmorow" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tomSmmmmmmmmorrrrrow" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tomSmmmmmmmmorrrrrow" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tomSmmmmmmmmorrrrrow" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tomSmmmmmmmmorrrrrow" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tomSmmmmmmmmorrrrrow" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tomSmmmmmmmmorrrrrow" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("t|mmrrrrrrro" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("t|mmrrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("t|mmrrrrrrro" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("t|mmrrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("t|mmrrrrrrro" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("t|mmrrrrrrro" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("dmmmmmmmmorrrrrrrrow" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("dmmmmmmmmorrrrrrrrow" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("dmmmmmmmmorrrrrrrrow" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("dmmmmmmmmorrrrrrrrow" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("dmmmmmmmmorrrrrrrrow" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("dmmmmmmmmorrrrrrrrow" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tmmmmWmmmorrw" =~ "^to?m+o?r+o?w?" :: Bool) `shouldBe` ("tmmmmWmmmorrw" TDFA.=~ "^to?m+o?r+o?w?" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmWmmmorrw" =~ "^to?m+o?r+o?w?" :: String) `shouldBe` ("tmmmmWmmmorrw" TDFA.=~ "^to?m+o?r+o?w?" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tmmmmWmmmorrw" =~ "^to?m+o?r+o?w?" :: (String, String, String)) `shouldBe` ("tmmmmWmmmorrw" TDFA.=~ "^to?m+o?r+o?w?" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("[[sbu!;\\S^0O!>\"Z;T!7]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[sbu!;\\S^0O!>\"Z;T!7]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[[sbu!;\\S^0O!>\"Z;T!7]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[sbu!;\\S^0O!>\"Z;T!7]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does match string" $ runTDFATest $ do
        ("[[sbu!;\\S^0O!>\"Z;T!7]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[sbu!;\\S^0O!>\"Z;T!7]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[[ibbbgo;j|{;-%%rKD$9((]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[ibbbgo;j|{;-%%rKD$9((]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[[ibbbgo;j|{;-%%rKD$9((]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[ibbbgo;j|{;-%%rKD$9((]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does match string" $ runTDFATest $ do
        ("[[ibbbgo;j|{;-%%rKD$9((]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[ibbbgo;j|{;-%%rKD$9((]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[[io!!obs;paM; Tf+y!r\"]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[io!!obs;paM; Tf+y!r\"]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[[io!!obs;paM; Tf+y!r\"]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[io!!obs;paM; Tf+y!r\"]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does match string" $ runTDFATest $ do
        ("[[io!!obs;paM; Tf+y!r\"]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[io!!obs;paM; Tf+y!r\"]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[[uousbb!;J_|:o\"|;>q:\\)v]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[uousbb!;J_|:o\"|;>q:\\)v]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[[uousbb!;J_|:o\"|;>q:\\)v]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[uousbb!;J_|:o\"|;>q:\\)v]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does match string" $ runTDFATest $ do
        ("[[uousbb!;J_|:o\"|;>q:\\)v]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[uousbb!;J_|:o\"|;>q:\\)v]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[[u;&M\"gj;0I5Utj]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[u;&M\"gj;0I5Utj]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[[u;&M\"gj;0I5Utj]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[u;&M\"gj;0I5Utj]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does match string" $ runTDFATest $ do
        ("[[u;&M\"gj;0I5Utj]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[u;&M\"gj;0I5Utj]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[[uoiuo;eGG3YD;_" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[uoiuo;eGG3YD;_" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[[uoiuo;eGG3YD;_" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[uoiuo;eGG3YD;_" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does match string" $ runTDFATest $ do
        ("[[uoiuo;eGG3YD;_" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[uoiuo;eGG3YD;_" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[[gibggos;JL;PIh]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[gibggos;JL;PIh]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[[gibggos;JL;PIh]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[gibggos;JL;PIh]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does match string" $ runTDFATest $ do
        ("[[gibggos;JL;PIh]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[gibggos;JL;PIh]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[[usb!guiug;qR>n;`T+]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[usb!guiug;qR>n;`T+]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[[usb!guiug;qR>n;`T+]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[usb!guiug;qR>n;`T+]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does match string" $ runTDFATest $ do
        ("[[usb!guiug;qR>n;`T+]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[usb!guiug;qR>n;`T+]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[[;_h_h@s$;2 &}]K" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[;_h_h@s$;2 &}]K" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[[;_h_h@s$;2 &}]K" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[;_h_h@s$;2 &}]K" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[[;_h_h@s$;2 &}]K" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[;_h_h@s$;2 &}]K" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[[ssAsO{NCe/Z;]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[ssAsO{NCe/Z;]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[[ssAsO{NCe/Z;]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[ssAsO{NCe/Z;]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[[ssAsO{NCe/Z;]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[ssAsO{NCe/Z;]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[[uuisgoi!;w}BCaS;z]]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[uuisgoi!;w}BCaS;z]]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[[uuisgoi!;w}BCaS;z]]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[uuisgoi!;w}BCaS;z]]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[[uuisgoi!;w}BCaS;z]]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[uuisgoi!;w}BCaS;z]]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[[Koii;J3=X\"*Eb;?]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[Koii;J3=X\"*Eb;?]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[[Koii;J3=X\"*Eb;?]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[Koii;J3=X\"*Eb;?]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[[Koii;J3=X\"*Eb;?]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[Koii;J3=X\"*Eb;?]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[[iugdg;sg-un3T*+0;N9z1|{]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool) `shouldBe` ("[[iugdg;sg-un3T*+0;N9z1|{]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[[iugdg;sg-un3T*+0;N9z1|{]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String) `shouldBe` ("[[iugdg;sg-un3T*+0;N9z1|{]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[[iugdg;sg-un3T*+0;N9z1|{]" =~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String)) `shouldBe` ("[[iugdg;sg-un3T*+0;N9z1|{]" TDFA.=~ "\\[\\[[!gbiuso]*;[^;]*;[^\\]]*\\]?$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("6367754" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: Bool) `shouldBe` ("6367754" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("6367754" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: String) `shouldBe` ("6367754" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("6367754" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: (String, String, String)) `shouldBe` ("6367754" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("7247365" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: Bool) `shouldBe` ("7247365" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("7247365" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: String) `shouldBe` ("7247365" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("7247365" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: (String, String, String)) `shouldBe` ("7247365" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("70531707" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: Bool) `shouldBe` ("70531707" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("70531707" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: String) `shouldBe` ("70531707" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("70531707" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: (String, String, String)) `shouldBe` ("70531707" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("187" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: Bool) `shouldBe` ("187" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("187" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: String) `shouldBe` ("187" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("187" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: (String, String, String)) `shouldBe` ("187" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("271o" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: Bool) `shouldBe` ("271o" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("271o" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: String) `shouldBe` ("271o" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("271o" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: (String, String, String)) `shouldBe` ("271o" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("6480182O" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: Bool) `shouldBe` ("6480182O" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("6480182O" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: String) `shouldBe` ("6480182O" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("6480182O" =~ "^(TEXT\\s*)?(\\d+|\\(\\s*\\d+\\s*\\))$" :: (String, String, String)) `shouldBe` ("6480182O" TDFA.=~ "^(TEXT[[:space:]]*)?([[:digit:]]+|\\([[:space:]]*[[:digit:]]+[[:space:]]*\\))$" :: (String, String, String))
    describe "Full matching" $ do
      it "does not match string" $ runTDFATest $ do
        ("." =~ "[\\u00CB\\u00C8\\u00CA]" :: Bool) `shouldBe` ("." TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("." =~ "[\\u00CB\\u00C8\\u00CA]" :: String) `shouldBe` ("." TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("." =~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String)) `shouldBe` ("." TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("$" =~ "[\\u00CB\\u00C8\\u00CA]" :: Bool) `shouldBe` ("$" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("$" =~ "[\\u00CB\\u00C8\\u00CA]" :: String) `shouldBe` ("$" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("$" =~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String)) `shouldBe` ("$" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("~" =~ "[\\u00CB\\u00C8\\u00CA]" :: Bool) `shouldBe` ("~" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("~" =~ "[\\u00CB\\u00C8\\u00CA]" :: String) `shouldBe` ("~" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("~" =~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String)) `shouldBe` ("~" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "[\\u00CB\\u00C8\\u00CA]" :: Bool) `shouldBe` ("f" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "[\\u00CB\\u00C8\\u00CA]" :: String) `shouldBe` ("f" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("f" =~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String)) `shouldBe` ("f" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("G" =~ "[\\u00CB\\u00C8\\u00CA]" :: Bool) `shouldBe` ("G" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("G" =~ "[\\u00CB\\u00C8\\u00CA]" :: String) `shouldBe` ("G" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("G" =~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String)) `shouldBe` ("G" TDFA.=~ "[\\u00CB\\u00C8\\u00CA]" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("xqo)K#&`On{" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("xqo)K#&`On{" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("xqo)K#&`On{" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("xqo)K#&`On{" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("xqo)K#&`On{" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("xqo)K#&`On{" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("P.e@itP#d" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("P.e@itP#d" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("P.e@itP#d" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("P.e@itP#d" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("P.e@itP#d" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("P.e@itP#d" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("LC6cfXq_c#8_" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("LC6cfXq_c#8_" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("LC6cfXq_c#8_" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("LC6cfXq_c#8_" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("LC6cfXq_c#8_" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("LC6cfXq_c#8_" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("su6g#Yo9t" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("su6g#Yo9t" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("su6g#Yo9t" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("su6g#Yo9t" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("su6g#Yo9t" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("su6g#Yo9t" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-d8by_r(#s.|n`B" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("-d8by_r(#s.|n`B" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-d8by_r(#s.|n`B" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("-d8by_r(#s.|n`B" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("-d8by_r(#s.|n`B" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("-d8by_r(#s.|n`B" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("QXM)J#;~)" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("QXM)J#;~)" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("QXM)J#;~)" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("QXM)J#;~)" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("QXM)J#;~)" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("QXM)J#;~)" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("bKRoj#=8" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("bKRoj#=8" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("bKRoj#=8" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("bKRoj#=8" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does match string" $ runTDFATest $ do
        ("bKRoj#=8" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("bKRoj#=8" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (".^@z1#YjZTLn" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` (".^@z1#YjZTLn" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (".^@z1#YjZTLn" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` (".^@z1#YjZTLn" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does not match string" $ runTDFATest $ do
        (".^@z1#YjZTLn" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` (".^@z1#YjZTLn" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("tPt}]'<(j" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("tPt}]'<(j" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("tPt}]'<(j" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("tPt}]'<(j" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("tPt}]'<(j" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("tPt}]'<(j" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("HX~7F;5 i?-" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("HX~7F;5 i?-" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("HX~7F;5 i?-" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("HX~7F;5 i?-" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("HX~7F;5 i?-" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("HX~7F;5 i?-" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Ux3m[3" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("Ux3m[3" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Ux3m[3" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("Ux3m[3" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Ux3m[3" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("Ux3m[3" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("^C8im32PI#YbIc" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("^C8im32PI#YbIc" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("^C8im32PI#YbIc" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("^C8im32PI#YbIc" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("^C8im32PI#YbIc" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("^C8im32PI#YbIc" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("X9R`(5#c6\"N[y7" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("X9R`(5#c6\"N[y7" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("X9R`(5#c6\"N[y7" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("X9R`(5#c6\"N[y7" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("X9R`(5#c6\"N[y7" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("X9R`(5#c6\"N[y7" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("2M.kfp*{bEm}" =~ "^[\\w.@_()-]+[#].*$" :: Bool) `shouldBe` ("2M.kfp*{bEm}" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("2M.kfp*{bEm}" =~ "^[\\w.@_()-]+[#].*$" :: String) `shouldBe` ("2M.kfp*{bEm}" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("2M.kfp*{bEm}" =~ "^[\\w.@_()-]+[#].*$" :: (String, String, String)) `shouldBe` ("2M.kfp*{bEm}" TDFA.=~ "^[[[:word:]].@_()-]+[#].*$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("4:-4" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("4:-4" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("4:-4" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("4:-4" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("4:-4" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("4:-4" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-0:7" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-0:7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-0:7" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-0:7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("-0:7" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-0:7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-3:2" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-3:2" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-3:2" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-3:2" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("-3:2" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-3:2" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("3:0" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("3:0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("3:0" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("3:0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("3:0" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("3:0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-9:-7" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-9:-7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-9:-7" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-9:-7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("-9:-7" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-9:-7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-0:-0" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-0:-0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-0:-0" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-0:-0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("-0:-0" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-0:-0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("3:-7" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("3:-7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("3:-7" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("3:-7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("3:-7" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("3:-7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-4:-6" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-4:-6" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-4:-6" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-4:-6" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("-4:-6" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-4:-6" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("2:1" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("2:1" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("2:1" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("2:1" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("2:1" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("2:1" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("-3:-6" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-3:-6" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("-3:-6" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-3:-6" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does match string" $ runTDFATest $ do
        ("-3:-6" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-3:-6" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("1C9" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("1C9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("1C9" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("1C9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("1C9" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("1C9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("o:9" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("o:9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("o:9" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("o:9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("o:9" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("o:9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<:-0" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("<:-0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<:-0" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("<:-0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<:-0" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("<:-0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("-4:6F" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-4:6F" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("-4:6F" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-4:6F" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("-4:6F" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-4:6F" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("9P-9" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("9P-9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("9P-9" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("9P-9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("9P-9" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("9P-9" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("-&:0" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-&:0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("-&:0" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-&:0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("-&:0" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-&:0" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("-7g7" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-7g7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("-7g7" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-7g7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("-7g7" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-7g7" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("-5:-1k" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("-5:-1k" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("-5:-1k" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("-5:-1k" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("-5:-1k" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("-5:-1k" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("+5:2" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("+5:2" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("+5:2" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("+5:2" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("+5:2" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("+5:2" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("9:-S" =~ "\\-?[0-9]:\\-?[0-9]" :: Bool) `shouldBe` ("9:-S" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("9:-S" =~ "\\-?[0-9]:\\-?[0-9]" :: String) `shouldBe` ("9:-S" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("9:-S" =~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String)) `shouldBe` ("9:-S" TDFA.=~ "\\-?[0-9]:\\-?[0-9]" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("\\" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("\\" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\\" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("\\" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("\\" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("\\" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("}" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("}" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("}" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("~" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("~" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("~" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("~" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("~" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("~" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("$" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("$" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("#" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("#" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("#" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("#" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("#" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("#" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("}" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("}" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("}" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("}" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("{" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("{" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("{" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("{" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does match string" $ runTDFATest $ do
        ("{" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("{" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        (">W" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` (">W" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        (">W" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` (">W" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does not match string" $ runTDFATest $ do
        (">W" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` (">W" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[B" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("[B" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[B" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("[B" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[B" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("[B" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("'Q" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("'Q" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("'Q" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("'Q" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("'Q" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("'Q" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("Y" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("Y" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("Y" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("Y" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("Y" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("Y" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("8" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("8" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("8" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("8" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("8" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("8" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("C" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("C" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("C" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("C" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("C" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("C" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("*%" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("*%" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("*%" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("*%" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("*%" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("*%" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("M" =~ "[^a-zA-Z0-9\\/\\+=]" :: Bool) `shouldBe` ("M" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("M" =~ "[^a-zA-Z0-9\\/\\+=]" :: String) `shouldBe` ("M" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: String)
      it "does not match string" $ runTDFATest $ do
        ("M" =~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String)) `shouldBe` ("M" TDFA.=~ "[^a-zA-Z0-9\\/\\+=]" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does match string" $ runTDFATest $ do
        ("exports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("exports.paths k {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths k {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("exports.paths k {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths k {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does not match string" $ runTDFATest $ do
        ("exports.paths k {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths k {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("exp_rts.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exp_rts.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("exp_rts.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exp_rts.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does not match string" $ runTDFATest $ do
        ("exp_rts.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exp_rts.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("eFports.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("eFports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("eFports.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("eFports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does not match string" $ runTDFATest $ do
        ("eFports.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("eFports.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("exportsGpaths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exportsGpaths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("exportsGpaths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exportsGpaths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does not match string" $ runTDFATest $ do
        ("exportsGpaths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exportsGpaths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("exports.paths'= {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths'= {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("exports.paths'= {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths'= {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does not match string" $ runTDFATest $ do
        ("exports.paths'= {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths'= {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("expoyts.paths = {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("expoyts.paths = {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("expoyts.paths = {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("expoyts.paths = {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does not match string" $ runTDFATest $ do
        ("expoyts.paths = {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("expoyts.paths = {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("exports.paths ` {" =~ "exports\\.paths = \\{" :: Bool) `shouldBe` ("exports.paths ` {" TDFA.=~ "exports\\.paths = \\{" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("exports.paths ` {" =~ "exports\\.paths = \\{" :: String) `shouldBe` ("exports.paths ` {" TDFA.=~ "exports\\.paths = \\{" :: String)
      it "does not match string" $ runTDFATest $ do
        ("exports.paths ` {" =~ "exports\\.paths = \\{" :: (String, String, String)) `shouldBe` ("exports.paths ` {" TDFA.=~ "exports\\.paths = \\{" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does match string" $ runTDFATest $ do
        ("it should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a lpgin hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a lpgin hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a lpgin hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a lpgin hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a lpgin hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a lpgin hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a logbn hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a logbn hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a logbn hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a logbn hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a logbn hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a logbn hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("it should fJil with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fJil with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("it should fJil with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fJil with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("it should fJil with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fJil with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a lYgin hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a lYgin hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a lYgin hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a lYgin hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a lYgin hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a lYgin hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a logi8 hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fail with a logi8 hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a logi8 hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fail with a logi8 hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("it should fail with a logi8 hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fail with a logi8 hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("it should Iail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should Iail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("it should Iail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should Iail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("it should Iail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should Iail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("t should fail with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("t should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("t should fail with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("t should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("t should fail with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("t should fail with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("it should fbil with a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should fbil with a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("it should fbil with a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should fbil with a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("it should fbil with a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should fbil with a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("it should failiwith a login hint" =~ "it should fail with a login hint" :: Bool) `shouldBe` ("it should failiwith a login hint" TDFA.=~ "it should fail with a login hint" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("it should failiwith a login hint" =~ "it should fail with a login hint" :: String) `shouldBe` ("it should failiwith a login hint" TDFA.=~ "it should fail with a login hint" :: String)
      it "does not match string" $ runTDFATest $ do
        ("it should failiwith a login hint" =~ "it should fail with a login hint" :: (String, String, String)) `shouldBe` ("it should failiwith a login hint" TDFA.=~ "it should fail with a login hint" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("[-/127)0h0!1/-/8879ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127)0h0!1/-/8879ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127)0h0!1/-/8879ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127)0h0!1/-/8879ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127)0h0!1/-/8879ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127)0h0!1/-/8879ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[-/127[0~0}1/-/8193ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127[0~0}1/-/8193ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127[0~0}1/-/8193ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127[0~0}1/-/8193ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127[0~0}1/-/8193ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127[0~0}1/-/8193ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[-/127N0$0s1/-/277151103ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127N0$0s1/-/277151103ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127N0$0s1/-/277151103ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127N0$0s1/-/277151103ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127N0$0s1/-/277151103ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127N0$0s1/-/277151103ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[-/127\"0l041/-/38ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127\"0l041/-/38ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127\"0l041/-/38ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127\"0l041/-/38ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127\"0l041/-/38ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127\"0l041/-/38ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[-/127%0+001/-/51ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127%0+001/-/51ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127%0+001/-/51ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127%0+001/-/51ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127%0+001/-/51ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127%0+001/-/51ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[-/127C0v0^1/-/7420520ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127C0v0^1/-/7420520ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127C0v0^1/-/7420520ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127C0v0^1/-/7420520ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127C0v0^1/-/7420520ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127C0v0^1/-/7420520ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[-/127e0#0R1/-/481629068ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127e0#0R1/-/481629068ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127e0#0R1/-/481629068ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127e0#0R1/-/481629068ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127e0#0R1/-/481629068ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127e0#0R1/-/481629068ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[-/127[0$0{1/-/413761ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127[0$0{1/-/413761ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127[0$0{1/-/413761ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127[0$0{1/-/413761ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127[0$0{1/-/413761ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127[0$0{1/-/413761ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("[-/127H0{0\"1/-/63ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127H0{0\"1/-/63ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("[-/127H0{0\"1/-/63ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127H0{0\"1/-/63ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does match string" $ runTDFATest $ do
        ("[-/127H0{0\"1/-/63ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127H0{0\"1/-/63ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("g-/127)0>0C1/-/0932ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("g-/127)0>0C1/-/0932ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("g-/127)0>0C1/-/0932ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("g-/127)0>0C1/-/0932ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("g-/127)0>0C1/-/0932ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("g-/127)0>0C1/-/0932ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/127H050O1/-/167ms GET /l/gger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127H050O1/-/167ms GET /l/gger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/127H050O1/-/167ms GET /l/gger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127H050O1/-/167ms GET /l/gger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/127H050O1/-/167ms GET /l/gger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127H050O1/-/167ms GET /l/gger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/127m0)0m1/-/083876K06ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127m0)0m1/-/083876K06ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/127m0)0m1/-/083876K06ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127m0)0m1/-/083876K06ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/127m0)0m1/-/083876K06ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127m0)0m1/-/083876K06ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/127V0p0H1/-/7591204y5ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127V0p0H1/-/7591204y5ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/127V0p0H1/-/7591204y5ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127V0p0H1/-/7591204y5ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/127V0p0H1/-/7591204y5ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127V0p0H1/-/7591204y5ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/127v0I031/-/59605187ms GET /loggEr] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127v0I031/-/59605187ms GET /loggEr] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/127v0I031/-/59605187ms GET /loggEr] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127v0I031/-/59605187ms GET /loggEr] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/127v0I031/-/59605187ms GET /loggEr] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127v0I031/-/59605187ms GET /loggEr] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/1q7=0>0:1/-/5ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/1q7=0>0:1/-/5ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/1q7=0>0:1/-/5ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/1q7=0>0:1/-/5ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/1q7=0>0:1/-/5ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/1q7=0>0:1/-/5ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/127L0`021/-/072054a4ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127L0`021/-/072054a4ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/127L0`021/-/072054a4ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127L0`021/-/072054a4ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/127L0`021/-/072054a4ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127L0`021/-/072054a4ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/127\\0*0!1/-/764075ms GET Nlogger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127\\0*0!1/-/764075ms GET Nlogger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/127\\0*0!1/-/764075ms GET Nlogger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127\\0*0!1/-/764075ms GET Nlogger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/127\\0*0!1/-/764075ms GET Nlogger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127\\0*0!1/-/764075ms GET Nlogger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/127'0V0 1/-/J02883ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127'0V0 1/-/J02883ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/127'0V0 1/-/J02883ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127'0V0 1/-/J02883ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/127'0V0 1/-/J02883ms GET /logger] aaa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127'0V0 1/-/J02883ms GET /logger] aaa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("[-/127j0V0g1/-/9987ms GET /logger] $aa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: Bool) `shouldBe` ("[-/127j0V0g1/-/9987ms GET /logger] $aa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("[-/127j0V0g1/-/9987ms GET /logger] $aa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: String) `shouldBe` ("[-/127j0V0g1/-/9987ms GET /logger] $aa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: String)
      it "does not match string" $ runTDFATest $ do
        ("[-/127j0V0g1/-/9987ms GET /logger] $aa" =~ "\\[-\\/127.0.0.1\\/-\\/\\d+ms GET \\/logger] aaa" :: (String, String, String)) `shouldBe` ("[-/127j0V0g1/-/9987ms GET /logger] $aa" TDFA.=~ "\\[-\\/127.0.0.1\\/-\\/[[:digit:]]+ms GET \\/logger] aaa" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("konqueror/66JN0mbQGk" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/66JN0mbQGk" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/66JN0mbQGk" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/66JN0mbQGk" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/66JN0mbQGk" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/66JN0mbQGk" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/0061584259BMQtk5" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/0061584259BMQtk5" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/0061584259BMQtk5" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/0061584259BMQtk5" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/0061584259BMQtk5" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/0061584259BMQtk5" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/3132198960h" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/3132198960h" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/3132198960h" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/3132198960h" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/3132198960h" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/3132198960h" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/tS4" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/tS4" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/tS4" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/tS4" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/tS4" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/tS4" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/9I" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/9I" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/9I" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/9I" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/9I" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/9I" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/_c4sxm" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/_c4sxm" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/_c4sxm" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/_c4sxm" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/_c4sxm" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/_c4sxm" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/189316EZuUfhkEJ" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/189316EZuUfhkEJ" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/189316EZuUfhkEJ" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/189316EZuUfhkEJ" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/189316EZuUfhkEJ" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/189316EZuUfhkEJ" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/49785cTaA_" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/49785cTaA_" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/49785cTaA_" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/49785cTaA_" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/49785cTaA_" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/49785cTaA_" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/AhehYs" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/AhehYs" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/AhehYs" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/AhehYs" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/AhehYs" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/AhehYs" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("konqueror/445908y9" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/445908y9" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("konqueror/445908y9" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/445908y9" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does match string" $ runTDFATest $ do
        ("konqueror/445908y9" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/445908y9" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("konq?eror/56257364jN9" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konq?eror/56257364jN9" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("konq?eror/56257364jN9" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konq?eror/56257364jN9" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("konq?eror/56257364jN9" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konq?eror/56257364jN9" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("ionqueror/8BmHf5C" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("ionqueror/8BmHf5C" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("ionqueror/8BmHf5C" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("ionqueror/8BmHf5C" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("ionqueror/8BmHf5C" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("ionqueror/8BmHf5C" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("konqu;ror/mMCwn2Y" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqu;ror/mMCwn2Y" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("konqu;ror/mMCwn2Y" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqu;ror/mMCwn2Y" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("konqu;ror/mMCwn2Y" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqu;ror/mMCwn2Y" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("konqueror/r>" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/r>" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("konqueror/r>" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/r>" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("konqueror/r>" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/r>" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("konquer4r/yHwkQd3V" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konquer4r/yHwkQd3V" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("konquer4r/yHwkQd3V" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konquer4r/yHwkQd3V" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("konquer4r/yHwkQd3V" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konquer4r/yHwkQd3V" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("konq+eror/hoGr5wbi" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konq+eror/hoGr5wbi" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("konq+eror/hoGr5wbi" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konq+eror/hoGr5wbi" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("konq+eror/hoGr5wbi" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konq+eror/hoGr5wbi" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("konqueror/70q flfhRA7" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqueror/70q flfhRA7" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("konqueror/70q flfhRA7" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqueror/70q flfhRA7" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("konqueror/70q flfhRA7" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqueror/70q flfhRA7" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("konqu<ror/cocJ" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konqu<ror/cocJ" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("konqu<ror/cocJ" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konqu<ror/cocJ" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("konqu<ror/cocJ" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konqu<ror/cocJ" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("konquero4/79056732p4xWEF" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: Bool) `shouldBe` ("konquero4/79056732p4xWEF" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("konquero4/79056732p4xWEF" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: String) `shouldBe` ("konquero4/79056732p4xWEF" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: String)
      it "does not match string" $ runTDFATest $ do
        ("konquero4/79056732p4xWEF" =~ "(konqueror)\\/((\\d+)?[\\w\\.]+)" :: (String, String, String)) `shouldBe` ("konquero4/79056732p4xWEF" TDFA.=~ "(konqueror)\\/(([[:digit:]]+)?[[[:word:]]\\.]+)" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("P.,;:s@\"]]]]]2e.,;:s@\"]]]]]]]]@vxMd" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("P.,;:s@\"]]]]]2e.,;:s@\"]]]]]]]]@vxMd" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("P.,;:s@\"]]]]]2e.,;:s@\"]]]]]]]]@vxMd" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("P.,;:s@\"]]]]]2e.,;:s@\"]]]]]]]]@vxMd" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("P.,;:s@\"]]]]]2e.,;:s@\"]]]]]]]]@vxMd" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("P.,;:s@\"]]]]]2e.,;:s@\"]]]]]]]]@vxMd" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("P.,;:s@\"]]]]ZP.,;:s@\"]]]]]]]]]];L.,;:s@\"]]]]]]]]]]a.,;:s@\"]]]]]]>L.,;:s@\"]]]]]]]]]`@.,;:s@\"]]]@[h8w01j4]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("P.,;:s@\"]]]]ZP.,;:s@\"]]]]]]]]]];L.,;:s@\"]]]]]]]]]]a.,;:s@\"]]]]]]>L.,;:s@\"]]]]]]]]]`@.,;:s@\"]]]@[h8w01j4]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("P.,;:s@\"]]]]ZP.,;:s@\"]]]]]]]]]];L.,;:s@\"]]]]]]]]]]a.,;:s@\"]]]]]]>L.,;:s@\"]]]]]]]]]`@.,;:s@\"]]]@[h8w01j4]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("P.,;:s@\"]]]]ZP.,;:s@\"]]]]]]]]]];L.,;:s@\"]]]]]]]]]]a.,;:s@\"]]]]]]>L.,;:s@\"]]]]]]]]]`@.,;:s@\"]]]@[h8w01j4]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("P.,;:s@\"]]]]ZP.,;:s@\"]]]]]]]]]];L.,;:s@\"]]]]]]]]]]a.,;:s@\"]]]]]]>L.,;:s@\"]]]]]]]]]`@.,;:s@\"]]]@[h8w01j4]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("P.,;:s@\"]]]]ZP.,;:s@\"]]]]]]]]]];L.,;:s@\"]]]]]]]]]]a.,;:s@\"]]]]]]>L.,;:s@\"]]]]]]]]]`@.,;:s@\"]]]@[h8w01j4]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("\".OI\"@DiVLm5Hou-sfC5AabbRdI-ZXP7L2OU=cnltXG" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("\".OI\"@DiVLm5Hou-sfC5AabbRdI-ZXP7L2OU=cnltXG" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\".OI\"@DiVLm5Hou-sfC5AabbRdI-ZXP7L2OU=cnltXG" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("\".OI\"@DiVLm5Hou-sfC5AabbRdI-ZXP7L2OU=cnltXG" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("\".OI\"@DiVLm5Hou-sfC5AabbRdI-ZXP7L2OU=cnltXG" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("\".OI\"@DiVLm5Hou-sfC5AabbRdI-ZXP7L2OU=cnltXG" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("\"ogx|\"@421$35g49F583]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("\"ogx|\"@421$35g49F583]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\"ogx|\"@421$35g49F583]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("\"ogx|\"@421$35g49F583]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("\"ogx|\"@421$35g49F583]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("\"ogx|\"@421$35g49F583]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("\"U<W\"@697^792229869]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("\"U<W\"@697^792229869]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\"U<W\"@697^792229869]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("\"U<W\"@697^792229869]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("\"U<W\"@697^792229869]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("\"U<W\"@697^792229869]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("\"91a3k!F\"@3^7I8752]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("\"91a3k!F\"@3^7I8752]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\"91a3k!F\"@3^7I8752]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("\"91a3k!F\"@3^7I8752]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("\"91a3k!F\"@3^7I8752]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("\"91a3k!F\"@3^7I8752]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("\"@E|\"@943p5V45C5]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("\"@E|\"@943p5V45C5]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("\"@E|\"@943p5V45C5]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("\"@E|\"@943p5V45C5]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does match string" $ runTDFATest $ do
        ("\"@E|\"@943p5V45C5]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("\"@E|\"@943p5V45C5]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("\"{G!i\"@S7ljx4iU-OFjuFE y" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("\"{G!i\"@S7ljx4iU-OFjuFE y" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("\"{G!i\"@S7ljx4iU-OFjuFE y" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("\"{G!i\"@S7ljx4iU-OFjuFE y" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("\"{G!i\"@S7ljx4iU-OFjuFE y" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("\"{G!i\"@S7ljx4iU-OFjuFE y" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("@.,;:s@\"]]]]]]]]]U.,;:s@\"]]]]]%U.1;:s@\"]]]]]]]]?|.,;:s@\"]]] 2.,;:s@\"]])p.,;:s@\"]]]]]]]pQ.,;:s@\"]]<Y.,;:s@\"]]]]]]]]do.,;:s@\"]]]]]@Vc89UOMWfU7aOnZiMWEfZJGaJwI" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("@.,;:s@\"]]]]]]]]]U.,;:s@\"]]]]]%U.1;:s@\"]]]]]]]]?|.,;:s@\"]]] 2.,;:s@\"]])p.,;:s@\"]]]]]]]pQ.,;:s@\"]]<Y.,;:s@\"]]]]]]]]do.,;:s@\"]]]]]@Vc89UOMWfU7aOnZiMWEfZJGaJwI" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("@.,;:s@\"]]]]]]]]]U.,;:s@\"]]]]]%U.1;:s@\"]]]]]]]]?|.,;:s@\"]]] 2.,;:s@\"]])p.,;:s@\"]]]]]]]pQ.,;:s@\"]]<Y.,;:s@\"]]]]]]]]do.,;:s@\"]]]]]@Vc89UOMWfU7aOnZiMWEfZJGaJwI" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("@.,;:s@\"]]]]]]]]]U.,;:s@\"]]]]]%U.1;:s@\"]]]]]]]]?|.,;:s@\"]]] 2.,;:s@\"]])p.,;:s@\"]]]]]]]pQ.,;:s@\"]]<Y.,;:s@\"]]]]]]]]do.,;:s@\"]]]]]@Vc89UOMWfU7aOnZiMWEfZJGaJwI" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("@.,;:s@\"]]]]]]]]]U.,;:s@\"]]]]]%U.1;:s@\"]]]]]]]]?|.,;:s@\"]]] 2.,;:s@\"]])p.,;:s@\"]]]]]]]pQ.,;:s@\"]]<Y.,;:s@\"]]]]]]]]do.,;:s@\"]]]]]@Vc89UOMWfU7aOnZiMWEfZJGaJwI" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("@.,;:s@\"]]]]]]]]]U.,;:s@\"]]]]]%U.1;:s@\"]]]]]]]]?|.,;:s@\"]]] 2.,;:s@\"]])p.,;:s@\"]]]]]]]pQ.,;:s@\"]]<Y.,;:s@\"]]]]]]]]do.,;:s@\"]]]]]@Vc89UOMWfU7aOnZiMWEfZJGaJwI" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("\"w=^ATh6/\"@['2u4m0=3]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("\"w=^ATh6/\"@['2u4m0=3]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("\"w=^ATh6/\"@['2u4m0=3]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("\"w=^ATh6/\"@['2u4m0=3]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("\"w=^ATh6/\"@['2u4m0=3]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("\"w=^ATh6/\"@['2u4m0=3]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("\"1&*<\"[gY?8RtYg2sohoLop-TAW[lTTkxEJZCBh1IsLkDyTJb5sBfcbBi8Wpj-4yDLyeSh14V-FlG3lzFIqjmYnA" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("\"1&*<\"[gY?8RtYg2sohoLop-TAW[lTTkxEJZCBh1IsLkDyTJb5sBfcbBi8Wpj-4yDLyeSh14V-FlG3lzFIqjmYnA" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("\"1&*<\"[gY?8RtYg2sohoLop-TAW[lTTkxEJZCBh1IsLkDyTJb5sBfcbBi8Wpj-4yDLyeSh14V-FlG3lzFIqjmYnA" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("\"1&*<\"[gY?8RtYg2sohoLop-TAW[lTTkxEJZCBh1IsLkDyTJb5sBfcbBi8Wpj-4yDLyeSh14V-FlG3lzFIqjmYnA" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("\"1&*<\"[gY?8RtYg2sohoLop-TAW[lTTkxEJZCBh1IsLkDyTJb5sBfcbBi8Wpj-4yDLyeSh14V-FlG3lzFIqjmYnA" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("\"1&*<\"[gY?8RtYg2sohoLop-TAW[lTTkxEJZCBh1IsLkDyTJb5sBfcbBi8Wpj-4yDLyeSh14V-FlG3lzFIqjmYnA" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("=.,;:;@\"]]]]]]]]]]S*.,;:s@\"]]~_.,;:s@\"]]]]]]\"`.,;:s@\"]]]@0,649124l336]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool) `shouldBe` ("=.,;:;@\"]]]]]]]]]]S*.,;:s@\"]]~_.,;:s@\"]]]]]]\"`.,;:s@\"]]]@0,649124l336]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("=.,;:;@\"]]]]]]]]]]S*.,;:s@\"]]~_.,;:s@\"]]]]]]\"`.,;:s@\"]]]@0,649124l336]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String) `shouldBe` ("=.,;:;@\"]]]]]]]]]]S*.,;:s@\"]]~_.,;:s@\"]]]]]]\"`.,;:s@\"]]]@0,649124l336]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: String)
      it "does not match string" $ runTDFATest $ do
        ("=.,;:;@\"]]]]]]]]]]S*.,;:s@\"]]~_.,;:s@\"]]]]]]\"`.,;:s@\"]]]@0,649124l336]" =~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String)) `shouldBe` ("=.,;:;@\"]]]]]]]]]]S*.,;:s@\"]]~_.,;:s@\"]]]]]]\"`.,;:s@\"]]]@0,649124l336]" TDFA.=~ "^(([^<>()[]\\.,;:s@\"]+(.[^<>()[]\\.,;:s@\"]+)*)|(\".+\"))@(([[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}])|(([a-zA-Z-0-9]+.)+[a-zA-Z]{2,}))$" :: (String, String, String))
    describe "Full matching" $ do
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/904/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/904/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/904/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/904/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/904/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/904/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/8584/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/8584/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/8584/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/8584/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/8584/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/8584/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/85151/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/85151/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/85151/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/85151/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/85151/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/85151/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4742257/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/4742257/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4742257/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/4742257/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4742257/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/4742257/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/106529190/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/106529190/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/106529190/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/106529190/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/106529190/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/106529190/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/85/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/85/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/85/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/85/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/85/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/85/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/503077560/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/503077560/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/503077560/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/503077560/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/503077560/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/503077560/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/7/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/7/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/7/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/7/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/7/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/7/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/3/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/3/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/3/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/3/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/3/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/3/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/3953/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/3953/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/3953/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/3953/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/3953/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/3953/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4296031/edit?lo=ale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/4296031/edit?lo=ale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4296031/edit?lo=ale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/4296031/edit?lo=ale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4296031/edit?lo=ale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/4296031/edit?lo=ale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/adm/n/posts/6036/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/adm/n/posts/6036/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/adm/n/posts/6036/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/adm/n/posts/6036/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/adm/n/posts/6036/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/adm/n/posts/6036/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/668891/edit?localV=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/668891/edit?localV=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/668891/edit?localV=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/668891/edit?localV=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/668891/edit?localV=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/668891/edit?localV=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/681505/edit?locale=en\">Hello World</a=" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/681505/edit?locale=en\">Hello World</a=" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/681505/edit?locale=en\">Hello World</a=" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/681505/edit?locale=en\">Hello World</a=" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/681505/edit?locale=en\">Hello World</a=" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/681505/edit?locale=en\">Hello World</a=" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4120276197/edit?locale=en\">Hello xorld</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/4120276197/edit?locale=en\">Hello xorld</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4120276197/edit?locale=en\">Hello xorld</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/4120276197/edit?locale=en\">Hello xorld</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/4120276197/edit?locale=en\">Hello xorld</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/4120276197/edit?locale=en\">Hello xorld</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/068939/edit?locale=en\">Hello Worlw</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/068939/edit?locale=en\">Hello Worlw</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/068939/edit?locale=en\">Hello Worlw</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/068939/edit?locale=en\">Hello Worlw</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/068939/edit?locale=en\">Hello Worlw</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/068939/edit?locale=en\">Hello Worlw</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/370219088/edit?locale=en\"KHello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/370219088/edit?locale=en\"KHello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/370219088/edit?locale=en\"KHello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/370219088/edit?locale=en\"KHello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/370219088/edit?locale=en\"KHello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/370219088/edit?locale=en\"KHello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a @ref=\"/admin/posts/59023/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a @ref=\"/admin/posts/59023/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a @ref=\"/admin/posts/59023/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a @ref=\"/admin/posts/59023/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a @ref=\"/admin/posts/59023/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a @ref=\"/admin/posts/59023/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posEs/496726/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posEs/496726/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posEs/496726/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posEs/496726/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posEs/496726/edit?locale=en\">Hello World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posEs/496726/edit?locale=en\">Hello World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/43/edit?locale=en\">Helvo World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: Bool) `shouldBe` ("<a href=\"/admin/posts/43/edit?locale=en\">Helvo World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: Bool)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/43/edit?locale=en\">Helvo World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: String) `shouldBe` ("<a href=\"/admin/posts/43/edit?locale=en\">Helvo World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: String)
      it "does not match string" $ runTDFATest $ do
        ("<a href=\"/admin/posts/43/edit?locale=en\">Helvo World</a>" =~ "<a href=\"/admin/posts/\\d+/edit\\?locale=en\">Hello World</a>" :: (String, String, String)) `shouldBe` ("<a href=\"/admin/posts/43/edit?locale=en\">Helvo World</a>" TDFA.=~ "<a href=\"/admin/posts/[[:digit:]]+/edit\\?locale=en\">Hello World</a>" :: (String, String, String))
