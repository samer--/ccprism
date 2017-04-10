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
   maximum a posterior, variational Bayes, and Viterbi learning. Deterministic
   annealling can be used with all of these methods.

   A couple of MCMC explanation sampling methods are also provided.

## Usage Examples

   You can load the test module included in the examples directory like this:
   ```
   swipl -g 'consult(pack(ccprism/examples/test)), ccp_test:init'
   ```
   More information on how to use the system to follow...

