type TransformerCh6<Entries, Acc = {}> = Entries extends [infer Entry, ...infer Rest]
  ? TransformerCh6<
      Rest,
      Entry extends [infer Key extends string | number | symbol, infer Value]
        ? Acc & { [K in Key]: Value }
        : Acc
    >
  : Acc

type Client = TransformerCh6<[['name', 'Jack'], ['id', 123]]>
