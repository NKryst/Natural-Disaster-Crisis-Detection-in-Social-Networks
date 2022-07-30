from gensim.models import FastText
import numpy as np
#loading model
model = FastText.load_fasttext_format('C:/Users/nickr/OneDrive/grcorpus_def.bin')
fasttext.load_model('C:/Users/nickr/OneDrive/grcorpus_def.bin')
print(model)