module Types where

-- definindo os tipos
type Value = Int
type Row a = [a]
type Matrix a = [Row a]
type Table = Matrix Value
type Choices = [Value]
