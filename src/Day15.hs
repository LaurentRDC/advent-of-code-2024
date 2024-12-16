{-# LANGUAGE OverloadedStrings #-}

import AOC
import Data.List qualified as List
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs)

main :: IO ()
main = do
  [inputFile] <- getArgs
  txt <- Text.readFile inputFile
  let (firstPart, secondPart) = Text.breakOn ("\n\n") txt

      (robot, boxes, walls) = parseWarehouse firstPart
      commands = parseCommands (Text.filter (/= '\n') secondPart)

  putStrLn $ "[Part 1] " <> show (part1 robot boxes walls commands)
  putStrLn $ "[Part 2] " <> show (part2 robot boxes walls commands)
  where
    -- Traversing the input 3x isn't great, but I'm busy today
    parseWarehouse :: Text -> (Position, Set Position, Set Position)
    parseWarehouse txt =
      ( head $
          [ (r, c)
          | (r, ls) <- zip [0 ..] (Text.lines txt),
            (c, chr) <- zip [0 ..] (Text.unpack ls),
            chr == '@'
          ],
        Set.fromList
          [ (r, c)
          | (r, ls) <- zip [0 ..] (Text.lines txt),
            (c, chr) <- zip [0 ..] (Text.unpack ls),
            chr == 'O'
          ],
        Set.fromList
          [ (r, c)
          | (r, ls) <- zip [0 ..] (Text.lines txt),
            (c, chr) <- zip [0 ..] (Text.unpack ls),
            chr == '#'
          ]
      )

    parseCommands :: Text -> [Command]
    parseCommands =
      map
        ( \c -> case c of
            '^' -> Up
            'v' -> Down
            '<' -> Lft
            '>' -> Rght
            _ -> error "oh no"
        )
        . Text.unpack

part1 :: Position -> Set Position -> Set Position -> [Command] -> Int
part1 rb bxs walls commands =
  sum $
    map toGPSCoord $
      Set.toList $
        go commands rb bxs
  where
    toGPSCoord :: Position -> Int
    toGPSCoord (r, c) = 100 * (fromIntegral r) + (fromIntegral c)

    go [] _ boxes = boxes
    go (command : rest) robot boxes =
      let (newRobot, newBoxes) = move command robot boxes
       in go rest newRobot newBoxes

    move :: Command -> Position -> Set Position -> (Position, Set Position)
    move cmd (rr, rc) boxes
      | (rr + vr, rc + vc) `Set.member` walls = ((rr, rc), boxes) -- No movement
      | (rr + vr, rc + vc) `Set.notMember` boxes = ((rr + vr, rc + vc), boxes) -- no boxes in the way
      | otherwise =
          let boxesInWay =
                Set.fromList $
                  List.unfoldr
                    ( \(r_, c_) ->
                        if (r_, c_) `Set.member` boxes
                          then Just ((r_, c_), (r_ + vr, c_ + vc))
                          else Nothing
                    )
                    (rr + vr, rc + vc)
           in if (not $ Set.null $ (Set.map (\(br, bc) -> (br + vr, bc + vc)) boxesInWay) `Set.intersection` walls)
                then ((rr, rc), boxes) -- No movement
                else ((rr + vr, rc + vc), Set.map (\(br, bc) -> if (br, bc) `Set.member` boxesInWay then (br + vr, bc + vc) else (br, bc)) boxes)
      where
        (vr, vc) = dir cmd

part2 :: Position -> Set Position -> Set Position -> [Command] -> Int
part2 (robotRow, robotCol) boxes_ walls_ commands =
  sum $
    map toGPSCoord $
      Set.toList $
        go commands (robotRow, 2 * robotCol) (Set.map (\(r, c) -> MkBox (r, 2 * c) (r, 2 * c + 1)) boxes_)
  where
    walls = Set.fromList $ concatMap (\(r, c) -> [(r, 2 * c), (r, 2 * c + 1)]) $ Set.toAscList walls_

    toGPSCoord :: Box -> Int
    toGPSCoord (MkBox (r, c) _) = 100 * (fromIntegral r) + (fromIntegral c)

    go [] _ boxes = boxes
    go (command : rest) robot boxes =
      let (newRobot, newBoxes) = move command robot boxes
       in go rest newRobot newBoxes

    move :: Command -> Position -> Set Box -> (Position, Set Box)
    move cmd (rr, rc) boxes
      | (rr + vr, rc + vc) `Set.member` walls = ((rr, rc), boxes) -- No movement
      | otherwise = case overlapsWithBox boxes (rr + vr, rc + vc) of
          Nothing -> ((rr + vr, rc + vc), boxes) -- no boxes in the way
          Just box ->
            let boxesInWay = relevantBoxes (Set.singleton box)
             in if conflictingWithWall (Set.map (moveBox (vr, vc)) boxesInWay)
                  then ((rr, rc), boxes) -- No movement
                  else
                    ( (rr + vr, rc + vc),
                      Set.map
                        ( \bx ->
                            if bx `Set.member` boxesInWay
                              then moveBox (vr, vc) bx
                              else bx
                        )
                        boxes
                    )
      where
        (vr, vc) = dir cmd

        conflictingWithWall :: Set Box -> Bool
        conflictingWithWall xs =
          not $
            Set.null $
              walls
                `Set.intersection` ( Set.fromList $
                                       concatMap (\(MkBox p1 p2) -> [p1, p2]) $
                                         Set.toList xs
                                   )

        relevantBoxes :: Set Box -> Set Box
        relevantBoxes acc =
          let overlapping = Set.fromList $ concatMap (\a -> [bx | bx <- Set.toList boxes, bx `overlaps` (moveBox (vr, vc) a)]) (Set.toList acc)
           in if Set.null (overlapping `Set.difference` acc)
                then overlapping `Set.union` acc
                else relevantBoxes (overlapping `Set.union` acc)
          where
            overlaps :: Box -> Box -> Bool
            overlaps (MkBox p1 p2) (MkBox p3 p4) =
              p1 == p3 || p1 == p4 || p2 == p3 || p2 == p4

data Box = MkBox Position Position
  deriving (Eq, Ord, Show)

moveBox :: (Row, Col) -> Box -> Box
moveBox (r, c) (MkBox (r1, c1) (r2, c2)) = MkBox (r1 + r, c1 + c) (r2 + r, c2 + c)

overlapsWithBox :: Set Box -> (Row, Col) -> Maybe Box
overlapsWithBox boxes pos = listToMaybe $ Set.toList $ Set.filter overlaps boxes
  where
    overlaps (MkBox p1 p2) = pos == p1 || pos == p2

data Command = Up | Down | Lft | Rght
  deriving (Eq, Ord, Show)

dir :: Command -> (Row, Col)
dir Up = (-1, 0)
dir Down = (1, 0)
dir Lft = (0, -1)
dir Rght = (0, 1)

type Position = (Row, Col)
