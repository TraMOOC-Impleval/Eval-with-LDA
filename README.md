# Eval-with-LDA

This repository contains five scripts used in the development of an implicit evaluation method for machine translations using LDA.

- extract_language_Coursera_simple.R: for the extraction of data in one language from a tab-separated matrix with columns for different languages.
- extract_language_Coursera_parallel.R: for the extraction of data in one language from a tab-separated matrix with columns for different languages, while also checking for content in a column containing data for another language.
- match_bilingual_topics_multimatched_weighted_with-commentary.R: for comparing two topic models and linking topics in one language to topics in the other.
- evaluate_translation_quality_with-commentary.R: for comparing the topical content of a source language text and its target language translation.
- babelnet_funcs_Dennis.py: for invoking the BabelNet API to look up target language translations of a source language word (Note: this script itself is in turn invoked by match_bilingual_topics_multimatched_weighted_with-commentary.R).
