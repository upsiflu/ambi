-- Modification types --
type Anchor                 = Anchor String   | BlankAnchor
type Input                  = Input  String   | BlankInput
type Media                  = Media  String   | BlankMedia
type Addition               = Increment       | Zero

increment : (Zipper Addition) -> (Zipper Addition)
increment additions = insert (Tree.singleton Increment) additions |>
  open ((==) Increment) |> Maybe.withDefault (Increment |> Tree.singleton |> Zipper.fromTree)

reduceA a = case (current a) of
  Anchor    s -> Anchor s
  BlankAnchor -> BlankAnchor

reduceI i = case (current i) of
  Input     s -> Input s
  BlankInput  -> BlankInput

reduceM m = case (current m) of
  Media     s -> Media s
  BlankMedia  -> BlankMedia

reduceC c = case (current c) of
  Just      t -> Just t
  Nothing     -> Nothing

-- Each Edit is saved in the DB.
Edit modification
   = Reset
   | Propose  (Moderate modification)
   | Undo     (Edit modification)
   | Published modification

-- To propose a modification, you have to moderate it first.
Moderate modification
   = Suggest modification
   | Assert modification
   | Accept modification
   | Withdraw modification
