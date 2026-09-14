# RSA models

WebPPL implementations of the four reasoning-based models in Section 3 of the paper. Each folder contains `model.wppl` (the model), `model-evaluation.R` (the script that runs the model through `rwebppl` and writes the pragmatic-listener distribution), and `data/PL.csv` (that distribution).

| Folder | Section | Model |
|---|---|---|
| `negation-know` | 3.1 | Negation model for *know* (Scontras & Tonhauser 2026) |
| `negation-stop` | 3.2 | Negation model for *stop* |
| `question-know` | 3.3 | Question model for *know* |
| `question-stop` | 3.4 | Question model for *stop* |

Parameter settings are those given in (8) of the paper. For each utterance, QUD bias, and prior, `PL.csv` gives the pragmatic listener's joint distribution over world states. The predictions in (9) are the probability that CC (for *know*) or PRE (for *stop*) is true after observing *neg-know*, *neg-stop*, *q-know*, or *q-stop*, averaged over the two QUD biases and the two priors except where (9) names a specific QUD or prior.

To run a model directly, `webppl model.wppl` after uncommenting one of the calls at the bottom of the file; `model-evaluation.R` is run from within its folder and sources `helpers.R` from the repository root.
