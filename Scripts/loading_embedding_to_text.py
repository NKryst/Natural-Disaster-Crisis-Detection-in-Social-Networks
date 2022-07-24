from gensim.models import word2vec
file_path = 'C:/Users/nickr/OneDrive/grcorpus_def.bin'
destination_file = 'C:/Users/nickr/OneDrive/vectors.text'
model = word2vec.Word2Vec.load_word2vec_format(file_path, binary=True)
model.save_word2vec_format(destination_file, binary=False)
