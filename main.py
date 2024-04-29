
import os
from typing import TYPE_CHECKING, Any, Sequence

from langchain.text_splitter import (
    Language,
    RecursiveCharacterTextSplitter,
)
from langchain.text_splitter import CharacterTextSplitter
from langchain_community.document_loaders import TextLoader
from langchain_community.vectorstores import FAISS
#from langchain_openai import OpenAIEmbeddings, OpenAI, PromptTemplate
from langchain import OpenAI, PromptTemplate
from langchain.embeddings import OpenAIEmbeddings
from langchain.retrievers import ContextualCompressionRetriever, BM25Retriever, EnsembleRetriever
from langchain.retrievers.document_compressors import LLMChainExtractor
from langchain.retrievers.document_compressors.chain_extract import NoOutputParser

from langchain.retrievers.document_compressors import DocumentCompressorPipeline

from langchain.retrievers.document_compressors.embeddings_filter import EmbeddingsFilter
from langchain_community.document_transformers.embeddings_redundant_filter import EmbeddingsRedundantFilter
from langchain_core.documents import BaseDocumentTransformer
from dotenv import load_dotenv
from langchain_core.runnables.config import run_in_executor

from langchain_core.documents import Document

from abc import ABC, abstractmethod

load_dotenv()

input_text = """
03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
               05  WRK-CMB-ITM2                    PIC X(200).
               05  WRK-CMB-ITM3                    PIC X(200).
               05  WRK-CMB-ITM4                    PIC X(200).
               05  WRK-CMB-ITM5                    PIC X(200).
"""

division_keywords=[
                # Split along divisions
                "IDENTIFICATION ",
                "ENVIRONMENT ",
                "DATA ",
                "PROCEDURE "
            ]

CHUNK_SIZE = 10000

def get_division_keywords():
    print("get_divsion_keywords\n")
    return division_keywords


prompt_template = """Given the following question and context, extract any part of the context *AS IS* that is relevant to answer the question. If none of the context is relevant return {no_output_str}."""

# Helper function for printing docs
def pretty_print_docs(docs):
    print(
        f"\n{'-' * 100}\n".join(
            [f"Document {i+1}:\n\n" + d.page_content for i, d in enumerate(docs)]
        )
    )


text = ""
with open('sample.cbl', 'r') as f:
   text = f.read()
text = [text]
splitter = RecursiveCharacterTextSplitter(
    separators=get_division_keywords(), 
    keep_separator=True,
    chunk_size=CHUNK_SIZE,
    chunk_overlap=0,
    add_start_index=True
    )
docs = splitter.create_documents(text)
print("After splitting documents\n")
print("----------------------------------------\n")
pretty_print_docs(docs)



embeddings = OpenAIEmbeddings(api_key=os.getenv('OPENAI_API_KEY'), chunk_size=CHUNK_SIZE)
embedding_reduntant_filter = EmbeddingsRedundantFilter(embeddings=embeddings, similarity_threshold=0.75)
# embedding_filter = EmbeddingsFilter(embeddings=embeddings, similarity_threshold=0.70)

faiss_retriever = FAISS.from_documents(docs, embeddings).as_retriever(
                            search_type="similarity_score_threshold",
                            search_kwargs={"score_threshold":0.50})

bm25_retriever = BM25Retriever.from_documents(docs)
bm25_retriever.k=1

pipeline_compressor = DocumentCompressorPipeline(
    transformers=[embedding_reduntant_filter]
)

ensemble_retriever = EnsembleRetriever(
    retrievers=[
        bm25_retriever
    ],
    weights=[0.5]
)

# Testing pipeline_compressor compress document result
result_docs = pipeline_compressor.compress_documents(docs, input_text)
print("Pipeline compressor\n")
print("-------------------\n")
pretty_print_docs(result_docs)


context_compression_retriever = ContextualCompressionRetriever(
    base_compressor=pipeline_compressor,
    base_retriever=ensemble_retriever
)

# Testing the Contextual compression documents
compressed_doc = context_compression_retriever.get_relevant_documents(input_text)
print("\nFinal Output\n")
print("--------------\n")
pretty_print_docs(compressed_doc)


