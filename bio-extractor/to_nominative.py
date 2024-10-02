import pymorphy2

# Initialize the morphological analyzer
morph = pymorphy2.MorphAnalyzer()

# Function to transform a word to the nominative case
def to_nominative(word, gender = 'masc'):
    parsed_word = morph.parse(word)[0]
    if 'nomn' in parsed_word.tag.grammemes:
        return word  # Already in nominative case
    nominative_form = parsed_word.inflect({'nomn', gender})
    if nominative_form:
        result_word = nominative_form.word
        if word[0].isupper():
            result_word = result_word.capitalize()
        return result_word
    return word  # Return the original word if nominative form is not found

def to_nominative_sentence(sentence):
    words = sentence.split()
    nominative_words = [to_nominative(word) for word in words]
    return ' '.join(nominative_words)
