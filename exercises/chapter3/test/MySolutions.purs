module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter ( eq street <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fn ln = not null <<< filter filterEntry
    where
      filterEntry :: Entry -> Boolean
      filterEntry e = e.firstName == fn && e.lastName == ln

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq compareEntries
    where
      compareEntries :: Entry -> Entry -> Boolean
      compareEntries a b = a.firstName == b.firstName && a.lastName == b.lastName
