import "hint" HLint.HLint

ignore "Eta reduce"        -- it is never good efficiency advice
ignore "Redundant bracket" -- CPP isn't good at avoiding prophylactic parens
ignore "Use camelCase"
