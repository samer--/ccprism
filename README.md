# ccprism: Probabilistic programming as a library using delimited control

   This package provides several services for working with probabilistic models and
   is based on the functionality of PRISM. Models are written as Prolog programs
   enriched with extra computational effects: probabilistic choice and tabling.
   Programs can be run in sampling mode or explanation mode. Explanation mode 
   results in a hypergraph representing the computation, which can then be processed
   to get:

      * inside probabilities (generalised sum-product algorithm)
      * the single best explanation (generalised Viterbi algorithm)
      * any number of explanations in order of probability (lazy k-best algorithm)
      * outside probabilities for computing parameter sufficient statistics
 
   Based on these several EM parameter learning methods are provided: maximum likelihood,
   maximum a posterior, variational Bayes, and (to come, probably) Viterbi learning.

## Testing

   You can load the test module included in the examples directory like this:
   ```
   swipl -O -g 'consult(pack(ccprism/examples/test))'
   ```

   @tbd
      - CLP R/Q
      - Automatic differentiation for counts?
      - Goal subsumption in tabling lookup
      - lazy explanation search, ccbeam etc
