# JCCI 2022 account-name coverage fixture

`source.tsv` is the research transcription of the Japan Chamber of Commerce and
Industry's 2022 Standard and Permitted Account List for levels 2 and 3. `A欄`
contains standard names and `B欄` contains permitted alternatives separated by
`|`. Parenthetical variants in the PDF are expanded to the concrete spellings
used by the parser test.

`source.tsv` keeps the six source-audit columns: level, division, A name, B
names, the pre-V-Land-2 EA mapping, and its audit status. `EXISTS`/`GAP` record
whether an exact constructor existed, `COLLAPSE` records an intentional
many-JCCI-accounts-to-one-EA mapping, and `AMBIGUOUS_LEGACY` records a modern
constructor colliding with a retained legacy alias. The last two columns are
mapping provenance, not fields transcribed from the PDF.

The `EA対応` column is a stale historical reference and is not the current
parser mapping. Land 4a gives the three level-2 A names `未収還付消費税`,
`未払固定資産税`, and `役員預り金` dedicated constructors.

`queries.tsv` freezes the adjudicated parser outcome for every distinct A/B
query after `normalizeTitle`: `right` for a unique constructor and `ambiguous`
for a deliberate candidate set. Its candidates describe the complete parser
contract: JCCI-derived aliases combined with pre-existing EA aliases. Therefore
an A name can remain ambiguous when the same spelling is also a B name, and
legacy constructors such as `Securities`, `InvestmentSecurities`, and
`Commutation` remain visible rather than being silently removed. Shared names
and generic gain/loss names are never resolved by first-match order.

The 21 frozen ambiguities are: `銀行預金`, `〇〇商店`, `貸付金`, `仮払金`,
`有価証券`, `投資有価証券`, `関係会社株式`, `借入金`, `未払金`, `仮受金`,
`営業収益`, `有価証券運用益`, `通信費`, `地代家賃`, `支払賃借料`,
`支払不動産賃借料`, `営業費用`, `有価証券運用損`, `為替差損益`,
`有価証券評価損益`, and `有価証券運用損益`. `〇〇商店` is a literal
counterparty placeholder in the source; real counterparty names do not become
aliases and must be mapped by the task context.

Coverage here is label-level parser coverage, not a claim that every JCCI A-row
has a distinct EA constructor. `COLLAPSE` rows deliberately merge some source
accounts, so an evaluation requiring those sub-ledger distinctions must declare
a coverage gap rather than treating the merged constructor as lossless.

Source:

- 日本商工会議所「商業簿記標準・許容勘定科目表」2022年版
  https://www.kentei.ne.jp/wp/wp-content/uploads/2021/12/2022_kamoku.pdf
  (accessed 2026-08-16; SHA-256
  `ad1691da158429dd5c4ec9a7721bb41ad2ecf55f9e6f27eb483b3722e0d0eb7e`)

The checked fixture is a transcription and mapping decision, not a modified
copy of the source PDF. `tools/gen-jcci-aliases.py` deterministically generates
the Haskell alias overlay from `queries.tsv`.
