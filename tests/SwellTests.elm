module SwellTests exposing (..)

import Direction exposing (Direction(..))
import Expect
import Swell
import Test exposing (..)


tests : Test
tests =
    describe "Swell Module tests suite"
        [ describe "swells to direction convertion tests"
            [ test "an empty array of swells should give an empty array of direction" <|
                \_ ->
                    []
                        |> Expect.equal (Swell.swellToDirection [])
            , test "an array containing a swell with direction of 0.0 should produce an array containing N for North" <|
                let
                    swell =
                        Swell.Swell 2.3 1 0.0 2.3 2
                in
                \_ ->
                    [ N ]
                        |> Expect.equal (Swell.swellToDirection [ swell ])
            , test
                "an array containing a swell with direction of 180.0 should produce an array containing S for South"
              <|
                let
                    swell =
                        Swell.Swell 2.3 1 180.0 2.3 2
                in
                \_ ->
                    [ S ]
                        |> Expect.equal (Swell.swellToDirection [ swell ])
            , test
                "an array containing 2 swells records should output the correct array of directions, DESC ordered with heighest height to slowest"
              <|
                let
                    swell1 =
                        Swell.Swell 2.3 1 23.0 2.3 2

                    swell2 =
                        Swell.Swell 2.0 1 175.6 2.3 1

                    swell3 =
                        Swell.Swell 8.1 1 284.1 4.3 3
                in
                \_ ->
                    [ W, NNE, SSE ]
                        |> Expect.equal (Swell.swellToDirection [ swell1, swell2, swell3 ])
            ]
        ]
