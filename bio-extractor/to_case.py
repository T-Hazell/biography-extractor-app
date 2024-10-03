import pymorphy2

# Initialize the morphological analyzer
morph = pymorphy2.MorphAnalyzer()

# Function to transform a word to another case
def to_case(word, gender = 'masc', case = 'nomn'):
    """
        Converts a given word to a specified grammatical case and gender.

        Parameters:
        word (str): The word to be converted.
        gender (str, optional): The grammatical gender to convert the word to. Defaults to 'masc'.
        case (str, optional): The grammatical case to convert the word to. Defaults to 'nomn'.

        Returns:
        str: The word converted to the specified case and gender. If the word is already in the specified case, 
             or if the case form is not found, the original word is returned.
    """
    parsed_word = morph.parse(word)[0]
    if case in parsed_word.tag.grammemes:
        return word  # Already in case
    case_form = parsed_word.inflect({case, gender})
    if case_form:
        result_word = case_form.word
        if word[0].isupper():
            result_word = result_word.capitalize()
        return result_word
    return word  # Return the original word if case form is not found

def to_case_sentence(sentence, gender = 'masc', case = 'nomn'):
    """
        Converts each word in a sentence to a specified grammatical case and gender.

        Args:
            sentence (str): The input sentence to be converted.
            gender (str, optional): The gender to which the words should be converted. Defaults to 'masc'.
            case (str, optional): The grammatical case to which the words should be converted. Defaults to 'nomn'.

        Returns:
            str: The sentence with each word converted to the specified case and gender.
    """
    words = sentence.split()
    case_words = [to_case(word, gender, case) for word in words]
    return ' '.join(case_words)
