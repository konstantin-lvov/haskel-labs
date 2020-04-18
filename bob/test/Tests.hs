{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.String       (fromString)

import Bob (responseFor)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "responseFor" $ for_ cases test
  where
    test Case{..} = it description $ responseFor (fromString input) `shouldBe` fromString expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "stating something"
               , input       = "По-ми-дор, по-мииии-дор."
               , expected    = "Побоку"
               }
        , Case { description = "shouting"
               , input       = "ОСТОРОЖНО!"
               , expected    = "Эй, расслабься!"
               }
        , Case { description = "shouting gibberish"
               , input       = "НРЛТРМНЕ"
               , expected    = "Эй, расслабься!"
               }
        , Case { description = "asking a question"
               , input       = "Эта криогенная камера меня полнит?"
               , expected    = "Точно"
               }
        , Case { description = "asking a numeric question"
               , input       = "Тебе, наверное, лет 15?"
               , expected    = "Точно"
               }
        , Case { description = "asking gibberish"
               , input       = "укеавпврф?"
               , expected    = "Точно"
               }
        , Case { description = "talking forcefully"
               , input       = "Пойдем разберемся за спортзалом!"
               , expected    = "Побоку"
               }
        , Case { description = "using acronyms in regular speech"
               , input       = "It's OK if you don't want to go to the DMV."
               , expected    = "Побоку"
               }
        , Case { description = "forceful question"
               , input       = "О ЧЕМ ТЫ ТОЛЬКО ДУМАЛ?"
               , expected    = "Остынь. Я знаю, что делаю!"
               }
        , Case { description = "shouting numbers"
               , input       = "1, 2, 3 ПОЕХАЛИ!"
               , expected    = "Эй, расслабься!"
               }
        , Case { description = "only numbers"
               , input       = "1, 2, 3"
               , expected    = "Побоку"
               }
        , Case { description = "question with only numbers"
               , input       = "4?"
               , expected    = "Точно"
               }
        , Case { description = "shouting with special characters"
               , input       = "ОМГ %^*@#$(*^ ЗОМБИ НАСТУПАЮТ!!11!!1!"
               , expected    = "Эй, расслабься!"
               }
        , Case { description = "shouting with no exclamation mark"
               , input       = "НЕНАВИЖУ"
               , expected    = "Эй, расслабься!"
               }
        , Case { description = "statement containing question mark"
               , input       = "Все, что кончается на ? означает вопрос."
               , expected    = "Побоку"
               }
        , Case { description = "non-letters with question"
               , input       = ":) ?"
               , expected    = "Точно"
               }
        , Case { description = "prattling on"
               , input       = "Стоп! Погоди. Все OK?"
               , expected    = "Точно"
               }
        , Case { description = "silence"
               , input       = ""
               , expected    = "Бред какой-то"
               }
        , Case { description = "prolonged silence"
               , input       = "          "
               , expected    = "Бред какой-то"
               }
        , Case { description = "alternate silence"
               , input       = "\t\t\t\t\t\t\t\t\t\t"
               , expected    = "Бред какой-то"
               }
        , Case { description = "multiple line question"
               , input       = "\nЭта криогенная камера меня полнит?\nНет."
               , expected    = "Побоку"
               }
        , Case { description = "starting with whitespace"
               , input       = "         хммммм..."
               , expected    = "Побоку"
               }
        , Case { description = "ending with whitespace"
               , input       = "Ничего если я люблю пробелы?   "
               , expected    = "Точно"
               }
        , Case { description = "other whitespace"
               , input       = "\n\r \t"
               , expected    = "Бред какой-то"
               }
        , Case { description = "non-question ending with whitespace"
               , input       = "Это выражение заканчивается пробелами      "
               , expected    = "Побоку"
               }
        ]
