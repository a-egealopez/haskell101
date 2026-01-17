import re
from math import sqrt
import math
from operator import index
from statistics import mean

alfabeto = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
number_letter_map = {numero:letra for numero,letra in enumerate(alfabeto)}
letter_number_map = {letra:numero for numero, letra in enumerate(alfabeto)}
alphSpecials = {
            'Á' : 'A',
            'É' : 'E',
            'Í' : 'I',
            'Ó' : 'O',
            'Ú' : 'U',
            'Ä' : 'A',
            'Ë' : 'E',
            'Ï' : 'I',
            'Ö' : 'O',
            'Ü' : 'U',
            'Ñ' : 'GN'
}

string = (
"UECWKDVLOTTVACKTPVGEZQMDAMRNPDDUXLBUICAMRHOECBHSPQLVIWO"
"FFEAILPNTESMLDRUURIFAEQTTPXADWIAWLACCRPBHSRZIVQWOFROGTT"
"NNXEVIVIBPDTTGAHVIACLAYKGJIEQHGECMESNNOCTHSGGNVWTQHKBPR"
"HMVUOYWLIAFIRIGDBOEBQLIGWARQHNLOISQKEPEIDVXXNETPAXNZGDX"
"WWEYQCTIGONNGJVHSQGEATHSYGSDVVOAQCXLHSPQMDMETRTMDUXTEQQ"
"JMFAEEAAIMEZREGIMUECICBXRVQRSMENNWTXTNSRNBPZHMRVRDYNECG"
"SPMEAVTENXKEQKCTTHSPCMQQHSQGTXMFPBGLWQZRBOEIZHQHGRTOBSG"
"TATTZRNFOSMLEDWESIWDRNAPBFOFHEGIXLFVOGUZLNUSRCRAZGZRTTA"
"YFEHKHMCQNTZLENPUCKBAYCICUBNRPCXIWEYCSIMFPRUTPLXSYCBGCC"
"UYCQJMWIEKGTUBRHVATTLEKVACBXQHGPDZEANNTJZTDRNSDTFEVPDXK"
"TMVNAIQMUQNOHKKOAQMTBKOFSUTUXPRTMXBXNPCLRCEAEOIAWGGVVUS"
"GIOEWLIQFOZKSPVMEBLOHLXDVCYSMGOPJEFCXMRUIGDXNCCRPMLCEWT"
"PZMOQQSAWLPHPTDAWEYJOGQSOAVERCTNQQEAVTUGKLJAXMRTGTIEAFW"
"PTZYIPKESMEAFCGJILSBPLDABNFVRJUXNGQSWIUIGWAAMLDRNNPDXGN"
"PTTGLUHUOBMXSPQNDKBDBTEECLECGRDPTYBVRDATQHKQJMKEFROCLXN"
"FKNSCWANNAHXTRGKCJTTRRUEMQZEAEIPAWEYPAJBBLHUEHMVUNFRPVM"
"EDWEKMHRREOGZBDBROGCGANIUYIBNZQVXTGORUUCUTNBOEIZHEFWNBI"
"GOZGTGWXNRHERBHPHGSIWXNPQMJVBCNEIDVVOAGLPONAPWYPXKEFKOC"
"MQTRTIDZBNQKCPLTTNOBXMGLNRRDNNNQKDPLTLNSUTAXMNPTXMGEZKA"
"EIKAGQ"
)
SPANISH_INDEX_COINCIDENCE = 0.0707

def possible_lengths(most_repeated):
    starting_positions = [m.start() for m in re.finditer(most_repeated, string)]
    distances = []
    for i in range(len(starting_positions) - 1):
        distances.append(starting_positions[i + 1] - starting_positions[i])
    #las longtides posibles del numero seran multiplos del mcd de las distancias.
    gcd = math.gcd(*distances)
    lengths = factorize(gcd)
    return lengths


def factorize(n):
    i = 2;
    number = n
    prime_numbers = [] #el propio numero tambien puede ser una posibilidad
    while i * i <= (n):
        if n % i == 0:
            prime_numbers.append(i)
            n = n // i
            while n % i == 0:
                n = n // i
        i = i + 1
    if n > 1: prime_numbers.append(n)
    if len(prime_numbers) > 1: prime_numbers.insert(0,n) #el propio numero tambien puede ser una posibilidad
    return prime_numbers
def kasiski_test(string):
    # recorremos todo el texto buscando frases repetidas de longitud 3
    mapa = {}
    for i in range(len(string) - 2):
        if mapa.get(string[i:i + 3]) is None:
            mapa[string[i:i + 3]] = 1
        else:
            mapa[string[i:i + 3]] += 1

    #filtramos la lista de frases repetidas para quedarnos solo con las que se repitan más de 2 veces
    trigrams = list((filter(lambda x: x[1] > 2, mapa.items())))

    sorted_trigrams = sorted(trigrams, key = lambda x : x[1], reverse = True)
    #ordenamos la lista para quedarnos con el más repetido  primero
    lengths = []
    if(len(sorted_trigrams) > 0):
        most_repeated = sorted_trigrams[0] #el trigram mas repetido
        lengths = possible_lengths(most_repeated[0]) #las posibles longitudes de la palabra clave
        print(lengths)
    else: #no hay elementos que se repitan
        print("La longitud del texto es demasiado corta")
    return lengths

frequencies = {} #mapa de las frecuencias de cada substring
substrings = [] #array de todos los substrings

def data_build(length, message):# funcion auxiliar que calcula los datos necesarios para aplicar la formula
    for i in range(length):
        substring_i = []
        map_i = {}
        for j in range(i, len(message),length):
            character = message[j]
            substring_i.append(message[j]) #construimos el substring a la vez que calculamos las frecuencias absolutas de cada letra para ese substring
            if map_i.get(character) is None:
                map_i[character] = 1
            else:
                map_i[character] += 1
        substrings.append(substring_i)
        frequencies[i] = map_i

def index_of_coincidence_test(length, message):

    data_build(length, message)

    print(substrings)
    print(len(substrings))
    print(frequencies)

    index_coincidence = []
    for i in range (length): #para cada subcadena
        acc = 0
        num_letters = 0
        for letter in alfabeto:
            frequency = frequencies[i].get(letter,0)
            acc += frequency * (frequency -1)
            num_letters += frequency
        index_coincidence.append((1/(num_letters*(num_letters-1))) * acc)

    return index_coincidence
def mutual_index_of_coincidence():
    #frequencias de cada letra en el idioma español
    p = {
        'A': 0.125125, 'B': 0.014181, 'C': 0.046735, 'D': 0.058518, 'E': 0.136609,
        'F': 0.006890, 'G': 0.010086, 'H': 0.006990, 'I': 0.062413, 'J': 0.004394,
        'K': 0.000200, 'L': 0.049631, 'M': 0.031456, 'N': 0.066999, 'O': 0.086679,
        'P': 0.025065, 'Q': 0.008788, 'R': 0.068605, 'S': 0.079689, 'T': 0.046235,
        'U': 0.039244, 'V': 0.008988, 'W': 0.000100, 'X': 0.002197, 'Y': 0.008988,
        'Z': 0.005193
    }


    final_word = []

    for substring in substrings: #para cada substring...

        result_map = {} # mapa con el MIC resultante de probar cada desplazamiento
        best_MIC = 0 #el desplazamiento que ha causado el mayor MIC
        best_shift = 0

        for i in range(len(alfabeto)): #probamos cada desplazamiento...
            frequency_map = {}
            for letter in substring:

                #desplazamos cada letra del substring
                associated_number = letter_number_map[letter]
                new_number = (associated_number + i) % len(alfabeto)
                associated_letter = number_letter_map[new_number]
                #calculamos las nuevas frecuencias de cada letra
                if frequency_map.get(associated_letter) is None:
                    frequency_map[associated_letter] = 1
                else: frequency_map[associated_letter] += 1

            MIC = 0
            f = 0
            for letter in alfabeto:
                if frequency_map.get(letter) is None:
                    f = 0
                else: f = frequency_map[letter]
                MIC += p[letter] * (f / len(substring))

            result_map[i] = MIC
            if MIC > best_MIC:
                best_MIC = MIC
                best_shift = i

        final_word.append(alfabeto[-best_shift % len(alfabeto)])
    return final_word

lengths = kasiski_test(string)
print(lengths)
indexes = index_of_coincidence_test(7, string)
print(indexes)
if mean(indexes) > SPANISH_INDEX_COINCIDENCE: print("La aproximacion es buena")
lista = mutual_index_of_coincidence()
print(lista)

key = lista
key_index = 0
deciphered_message = []
for char in string:
    deciphered_char_num = (letter_number_map[char] - letter_number_map[key[key_index]]) % len(alfabeto)
    deciphered_char = alfabeto[deciphered_char_num]
    deciphered_message.append(deciphered_char)
    key_index = (key_index + 1)% len(key)
result = "".join(deciphered_message)
print(result)
