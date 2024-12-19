

<h2>Local</h2>

1. Convert document to plain text paragraphs; impose minimum size on the paragraphs
   (gollama ollama)  (ingest-doc doc id model uri top-dir) 
                                     
									 


<h2>Remote</h2>

2. Calculate embeddings for each paragraph; store as json in <top-dir>/db/
    (gollama ollama)   (recurse-process-para id para counter plst elst model uri top-dir) gets embeddings

3. Calculate embedding for query


4. Calculate similarity of query embedding with each paragraph; sort

5. Extract top N most similar paragraphs

6. Append to system prompt and submit


<h2>File naming conventions</h2>

<h3> <top-dir>/text directory</h3>

MyFile.txt  original ascii text file

<h3> <top-dir>/db directory</h3>

|File name|Description|
|-----|----|
|indexed-paragraphs.json  |original file with small paragraphs (< 20 tokens) combined into normalized and indexed paragraphs|
|embedded-paragraphs.json |embeddings for associated indexed-paragraphs.json file matched by index|
|queries.json |plain text queries in json format|
|indexed-queries.json |indexed plain text queries|
|embedded-queries.json |indexed embeddings for queries|
|system-prompts.json | system prompts in json format||
|indexed-system-prompts.json | system prompts, indexed in json format||
|embedded-system-prompts.json |indexed embeddings for system prompts|


<h2>db.json</h2>

[{"id":"acbf1a82b78d",<br>
"doc":"/home/mbc/projects/gollama/text/ppan.txt",<br>
"title":"ppan",<br>
"model":"mistral:latest",<br>
"algorithm":"cosine-sim",<br>
"date":"241213092503"}]<br>
