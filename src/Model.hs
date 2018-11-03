{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Map.Strict (Map, foldlWithKey)
import Data.List.NonEmpty

type Address = String

data Community a = CommunityRef Address

data Member a = MemberRef Address
  deriving (Eq, Show)

class Registry f a where

  isMember :: Community a -> Member a -> f Bool

  load :: Community a -> Member a -> f (Maybe a)

  loadAll :: Community a -> f [a]



type Influence = Rational

class InfluenceDistribution f a method where

  distribute :: Registry f a => Community a -> Member a -> method -> f Influence



data Democratic = Democratic

instance Functor f => InfluenceDistribution f a Democratic where

  distribute community member _ = fmap doAllocation (isMember community member)
    where doAllocation True = 1.0
          doAllocation False = 0.0



data Totalitarian a = Dictator (Member a)

instance Applicative f => InfluenceDistribution f a (Totalitarian a) where

  distribute _ member (Dictator dic) = pure $ if member == dic then 1.0 else 0.0



type Location = String

type InverseDistanceWeight = Rational

data GeolocationBased = GeoBased Location

instance Functor f => InfluenceDistribution f (Location -> InverseDistanceWeight) GeolocationBased where

  distribute community member (GeoBased location) = fmap doAllocation (load community member)
    where doAllocation (Just weight) = weight location
          doAllocation Nothing = 0.0



data InfluenceAllocation p = InflAlloc (Map p Influence)

class DecisionResolution p method where
  
  resolve :: InfluenceAllocation p -> Influence -> method -> [p]



data Majority = Majority

instance DecisionResolution p Majority where

  resolve (InflAlloc mapping) _ _ = fst result
    where result = foldlWithKey combine ([], 0.0) mapping
          combine :: ([p], Influence) -> p -> Influence -> ([p], Influence)
          combine (lastProposals, lastInfl) proposal influence
            | lastInfl < influence = ([proposal], influence)
            | lastInfl == influence && lastInfl /= 0.0 = ((proposal : lastProposals), influence)
            | otherwise = ([], 0.0)



data BinaryProposal = Yes | No

                             -- A number in [1, 0)
data LowInertiaResolution = LIR Rational

instance DecisionResolution BinaryProposal LowInertiaResolution where

  resolve (InflAlloc mapping) total (LIR required) =
      if accumulated >= requiredInfluence then [Yes] else [No]
    where requiredInfluence = total * required
          accumulated = foldlWithKey combine 0.0 mapping
          combine :: Influence -> BinaryProposal -> Influence -> Influence
          combine acc Yes x = acc + x
          combine acc No _ = acc



data Ballot p = Ballot (NonEmpty p)

data Vote p a = Vote (Ballot p) (Member a) (InfluenceAllocation p)

data BallotBox p = BallotBoxRef Address

data ProofOfVote p a = ProofOfVoteRef Address (Member a)

class Collector f p where

  create :: Ballot p -> f (BallotBox p)

  vote :: Vote p a -> BallotBox p -> f (ProofOfVote p a)

  inspect :: BallotBox p -> f (InfluenceAllocation p)

  close :: BallotBox p -> f ()
