# RSA models

WebPPL implementations of the four models in Section 3 of the paper. The negation model for *know* is the one in Scontras & Tonhauser (2025). Each folder contains `model.wppl` (the model), `model-evaluation.R` (the script that runs the model through `rwebppl` and writes the pragmatic-listener distribution), and `data/PL.csv` (that distribution).

| Folder | Section | Model |
|---|---|---|
| `negation-know` | 3.1 | Negation model for *know* |
| `negation-stop` | 3.2 | Negation model for *stop* |
| `question-know` | 3.3 | Question model for *know* |
| `question-stop` | 3.4 | Question model for *stop* |

To run a model directly, `webppl model.wppl` after uncommenting one of the calls at the bottom of the file; `model-evaluation.R` is run from within its folder and sources `helpers.R` from the repository root.
