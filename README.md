

<h2>Local</h2>

1. Convert document to plain text paragraphs; impose minimum size on the paragraphs
   (gollama ollama)  (ingest-doc doc id model uri top-dir) 
                                        |---> (recurse-process-para para counter plst elst model uri) gets embeddings


<h2>Remote</h2>

2. Calculate embeddings for each paragraph; store as json in <top-dir>/db/
    (gollama ollama)   (recurse-process-para id para counter plst elst model uri top-dir) gets embeddings

3. Calculate embedding for query


4. Calculate similarity of query embedding with each paragraph; sort

5. Extract top N most similar paragraphs

6. Append to system prompt and submit
