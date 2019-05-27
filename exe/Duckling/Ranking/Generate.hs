-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module Duckling.Ranking.Generate
  ( regenAllClassifiers
  , regenClassifiers
  , regenLangClassifiers
  ) where

import Data.HashSet (HashSet)
import Language.Haskell.Exts as F
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Ranking.Train
import Duckling.Ranking.Types
import Duckling.Rules
import Duckling.Testing.Types
import qualified Duckling.Time.AR.Corpus as ARTime
import qualified Duckling.Time.DA.Corpus as DATime
import qualified Duckling.Time.DE.Corpus as DETime
import qualified Duckling.Time.EL.Corpus as ELTime
import qualified Duckling.Time.EN.Corpus as ENTime
import qualified Duckling.Time.EN.CA.Corpus as EN_CATime
import qualified Duckling.Time.EN.GB.Corpus as EN_GBTime
import qualified Duckling.Time.EN.US.Corpus as EN_USTime
import qualified Duckling.Time.ES.Corpus as ESTime
import qualified Duckling.Time.FR.Corpus as FRTime
import qualified Duckling.Time.GA.Corpus as GATime
import qualified Duckling.Time.HR.Corpus as HRTime
import qualified Duckling.Time.HE.Corpus as HETime
import qualified Duckling.Time.HU.Corpus as HUTime
import qualified Duckling.Time.IT.Corpus as ITTime
import qualified Duckling.Time.KO.Corpus as KOTime
import qualified Duckling.Time.NB.Corpus as NBTime
import qualified Duckling.Time.NL.Corpus as NLTime
import qualified Duckling.Time.NL.BE.Corpus as NL_BETime
import qualified Duckling.Time.PL.Corpus as PLTime
import qualified Duckling.Time.PT.Corpus as PTTime
import qualified Duckling.Time.RO.Corpus as ROTime
import qualified Duckling.Time.SV.Corpus as SVTime
import qualified Duckling.Time.UK.Corpus as UKTime
import qualified Duckling.Time.VI.Corpus as VITime
import qualified Duckling.Time.ZH.Corpus as ZHTime
import qualified Duckling.Time.ZH.CN.Corpus as ZH_CNTime
import qualified Duckling.Time.ZH.HK.Corpus as ZH_HKTime
import qualified Duckling.Time.ZH.MO.Corpus as ZH_MOTime
import qualified Duckling.Time.ZH.TW.Corpus as ZH_TWTime

-- -----------------------------------------------------------------
-- Main

regenAllClassifiers :: IO ()
regenAllClassifiers = do
  -- Regen default classifiers for langs
  mapM_ (regenClassifiers . defaultLocale) [minBound .. maxBound]
  -- Regen classifiers for locales
  mapM_ regenClassifiers locales
  where
    defaultLocale :: Lang -> Locale
    defaultLocale lang = makeLocale lang Nothing
    f :: [Locale] -> Lang -> HashSet Region -> [Locale]
    f res lang countries =
      res ++ [makeLocale lang (Just c) | c <- HashSet.toList countries]
    locales = HashMap.foldlWithKey' f [] allLocales

regenLangClassifiers :: Lang -> IO ()
regenLangClassifiers lang = do
  regenClassifiers $ makeLocale lang Nothing
  mapM_ (regenClassifiers . makeLocale lang . Just)
    $ HashMap.lookupDefault HashSet.empty lang allLocales

-- | Run this function to overwrite the file with Classifiers data
regenClassifiers :: Locale -> IO ()
regenClassifiers locale = do
  putStrLn $ "Regenerating " ++ filepath ++ "..."
  writeFile filepath $
    (headerComment ++) $
    prettyPrintWithMode baseMode $ (noLoc <$) m
  putStrLn "Done!"
  where
    moduleName = show locale

    filepath = "Duckling/Ranking/Classifiers/" ++ moduleName ++ ".hs"

    rules = rulesFor locale . HashSet.singleton $ This Time

    -- | The trained classifier to write out
    classifiers = makeClassifiers rules trainSet

    -- | The training set (corpus)
    trainSet = getCorpus locale

    -- Data structure for the module
    m = Module () (Just header) pragmas imports decls

    -- Declares the top level options pragma
    pragmas = [ LanguagePragma () [Ident () "OverloadedStrings"] ]

    -- Declares the header for the module
    -- "module Duckling.Ranking.Classifiers (classifiers) where"
    header = ModuleHead ()
      (ModuleName () $ "Duckling.Ranking.Classifiers." ++ moduleName)
      Nothing $
      Just $ ExportSpecList ()
       [ EVar () (unQual "classifiers")
       ]

    -- All imports the file will need
    imports =
      [ genImportModule "Data.String"
      , genImportModule "Prelude"
      , (genImportModule "Data.HashMap.Strict")
        { importQualified = True
        , importAs = Just (ModuleName () "HashMap")
        }
      , genImportModule "Duckling.Ranking.Types"
      ]

    -- The code body
    decls =
      [ -- Type Signature
        TypeSig () [Ident () "classifiers"] (TyCon () (unQual "Classifiers"))
        -- function body
      , FunBind ()
          [ Match () (Ident () "classifiers") []
              (UnGuardedRhs () (genList classifiers)) Nothing
          ]
      ]

    headerComment :: String
    headerComment = "\
\-- Copyright (c) 2016-present, Facebook, Inc.\n\
\-- All rights reserved.\n\
\--\n\
\-- This source code is licensed under the BSD-style license found in the\n\
\-- LICENSE file in the root directory of this source tree. An additional grant\n\
\-- of patent rights can be found in the PATENTS file in the same directory.\n\n\
\-----------------------------------------------------------------\n\
\-- Auto-generated by regenClassifiers\n\
\--\n\
\-- DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n\
\--  @" ++ "generated\n\
\-----------------------------------------------------------------\n"

-- | Retrieves the corpus for the locale.
-- The corpus is all the language examples + all the locale examples, if any.
-- Locales don't provide a corpus as contexts need to agree.
getCorpus :: Locale -> Corpus
getCorpus (Locale lang Nothing) = getDefaultCorpusForLang lang
getCorpus locale@(Locale lang (Just region)) =
  withLocale (getCorpusForLang lang) locale $ getExamplesForLocale lang region

-- | For backward compatibility.
getDefaultCorpusForLang :: Lang -> Corpus
getDefaultCorpusForLang EN = ENTime.defaultCorpus
getDefaultCorpusForLang NL = NLTime.defaultCorpus
getDefaultCorpusForLang lang = getCorpusForLang lang

getCorpusForLang :: Lang -> Corpus
getCorpusForLang AR = ARTime.corpus
getCorpusForLang BG = (testContext, testOptions, [])
getCorpusForLang BN = (testContext, testOptions, [])
getCorpusForLang CS = (testContext, testOptions, [])
getCorpusForLang DA = DATime.corpus
getCorpusForLang DE = DETime.corpus
getCorpusForLang EL = ELTime.corpus
getCorpusForLang EN = ENTime.corpus
getCorpusForLang ES = ESTime.corpus
getCorpusForLang ET = (testContext, testOptions, [])
getCorpusForLang FI = (testContext, testOptions, [])
getCorpusForLang FR = FRTime.corpus
getCorpusForLang GA = GATime.corpus
getCorpusForLang HR = HRTime.corpus
getCorpusForLang HE = HETime.corpus
getCorpusForLang HU = HUTime.corpus
getCorpusForLang HI = (testContext, testOptions, [])
getCorpusForLang ID = (testContext, testOptions, [])
getCorpusForLang IS = (testContext, testOptions, [])
getCorpusForLang IT = ITTime.corpus
getCorpusForLang JA = (testContext, testOptions, [])
getCorpusForLang KA = (testContext, testOptions, [])
getCorpusForLang KM = (testContext, testOptions, [])
getCorpusForLang KN = (testContext, testOptions, [])
getCorpusForLang KO = KOTime.corpus
getCorpusForLang LO = (testContext, testOptions, [])
getCorpusForLang ML = (testContext, testOptions, [])
getCorpusForLang MN = (testContext, testOptions, [])
getCorpusForLang MY = (testContext, testOptions, [])
getCorpusForLang NB = NBTime.corpus
getCorpusForLang NE = (testContext, testOptions, [])
getCorpusForLang NL = NLTime.corpus
getCorpusForLang PL = PLTime.corpus
getCorpusForLang PT = PTTime.corpus
getCorpusForLang RO = ROTime.corpus
getCorpusForLang RU = (testContext, testOptions, [])
getCorpusForLang SV = SVTime.corpus
getCorpusForLang SW = (testContext, testOptions, [])
getCorpusForLang TA = (testContext, testOptions, [])
getCorpusForLang TR = (testContext, testOptions, [])
getCorpusForLang UK = UKTime.corpus
getCorpusForLang VI = VITime.corpus
getCorpusForLang ZH = ZHTime.corpus

getExamplesForLocale :: Lang -> Region -> [Example]
getExamplesForLocale EN CA = EN_CATime.allExamples
getExamplesForLocale EN GB = EN_GBTime.allExamples
getExamplesForLocale EN US = EN_USTime.allExamples
getExamplesForLocale NL BE = NL_BETime.allExamples
getExamplesForLocale ZH CN = ZH_CNTime.allExamples
getExamplesForLocale ZH HK = ZH_HKTime.allExamples
getExamplesForLocale ZH MO = ZH_MOTime.allExamples
getExamplesForLocale ZH TW = ZH_TWTime.allExamples
getExamplesForLocale _ _   = []

-- -----------------------------------------------------------------
-- Source generators

-- | Generates a line for an import
--
-- `genImportModule "Foo.Bar"` spits out:
-- "import Foo.Bar" in the code
genImportModule :: String -> ImportDecl ()
genImportModule name = ImportDecl
  { importAnn = ()
  , importModule = ModuleName () name
  , importQualified = False
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs = Nothing
  }

-- | Creates the expression to build the HashMap object
genList :: Classifiers -> Exp ()
genList cs = appFromList $ map genClassifier $ HashMap.toList cs
  where
    -- "fromList ..."
    appFromList exprs = App ()
      (Var () (Qual () (ModuleName () "HashMap") (Ident () "fromList")))
      (List () exprs)

    -- ("name", Classifier { okData ....
    genClassifier (name, Classifier{..}) =
      let uname = Text.unpack name in
      Tuple () Boxed
        [ Lit () $ F.String () uname uname
        , RecConstr () (unQual "Classifier")
            [ genClassData okData "okData"
            , genClassData koData "koData"
            ]
        ]

    -- ClassData { prior = -0.123, unseen = ...
    genClassData ClassData{..} name = FieldUpdate () (unQual name) $
      RecConstr () (unQual "ClassData")
        [ FieldUpdate () (unQual "prior") $ floatSym prior
        , FieldUpdate () (unQual "unseen") $ floatSym unseen
        , FieldUpdate () (unQual "likelihoods") $
            appFromList $ map genLikelihood $ HashMap.toList likelihoods
        , FieldUpdate () (unQual "n") $
            Lit () (Int () (fromIntegral n) (show n))
        ]

    -- ("feature", 0.0)
    genLikelihood (f, d) =
      let uf = Text.unpack f in
      Tuple () Boxed
        [ Lit () $ F.String () uf uf
        , floatSym d
        ]

-- Helper to print out doubles
floatSym :: Double -> Exp ()
floatSym val
  | isInfinite val = if val < 0
      then NegApp () inf
      else inf
  | otherwise = Lit () (Frac () (realToFrac val) $ show val)
  where
    inf = Var () $ unQual "infinity"

-- Helper for unqualified things
unQual :: String -> QName ()
unQual name = UnQual () (Ident () name)


-- -----------------------------------------------------------------
-- Printing helpers

baseMode :: PPHsMode
baseMode = PPHsMode
  { classIndent   = 2
  , doIndent      = 3
  , multiIfIndent = 3
  , caseIndent    = 2
  , letIndent     = 2
  , whereIndent   = 2
  , onsideIndent  = 2
  , spacing       = True
  , layout        = PPOffsideRule
  , linePragmas   = False
  }
