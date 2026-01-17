from itertools import chain

alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789;:,.-'
number_letter_map = {numero:letra for numero,letra in enumerate(alphabet)}
letter_number_map = {letra:numero for numero, letra in enumerate(alphabet)}
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
key = "HOLA"

message = """El amanecer sobre la bahía fue lento y perezoso. Las luces de la ciudad aún parpadeaban en la distancia cuando las primeras embarcaciones comenzaron a cortar la niebla con su estela. Había un olor a sal y a algas que se mezclaba con el café recién hecho de los puestos del muelle, donde pescadores y comerciantes intercambiaban saludos y pequeñas historias de la noche anterior. Entre los muros de piedra, las gaviotas reclamaban su porción de jornada mientras los faros, como centinelas, guiaban a quien necesitase volver a puerto.

En el centro de la ciudad, la plaza mayor despertaba con un murmullo constante: vendedores acomodaban toldos, bicicletas cruzaban en línea recta sorteando charcos y grupos de estudiantes discutían proyectos y exámenes con entusiasmo contenido. Las fachadas, algunas recién pintadas y otras con la pátina del tiempo, contaban silenciosamente la historia de generaciones que habían pasado por las mismas esquinas. Un anciano, sentado en un banco con un periódico doblado, observaba el flujo de transeúntes con la calma de quien ha aprendido a distinguir lo urgente de lo verdaderamente importante.

Al otro extremo, un barrio industrial dejaba tras de sí el eco de máquinas y trabajo. Allí, las naves abandonadas habían sido reutilizadas como talleres, estudios y pequeños teatros de calle. Artistas y artesanos transformaban materiales olvidados en objetos con alma: esculturas metálicas que, al reflejar la luz, competían con las ventanas modernas que se alzaban a modo de testigos del cambio. En esos talleres se tejían también conversaciones sobre el futuro de la ciudad, sobre cómo conciliar la memoria colectiva con la atracción de la innovación.

En un parque cercano, niños y niñas corrían detrás de una pelota mientras sus familias extendían mantas y compartían comidas sencillas. Había quienes trajeron libros, quienes descansaban con los ojos cerrados y quienes aprovechaban para enseñar a los más pequeños a reconocer ciertas aves o a plantar una pequeña semilla en un macetero. La naturaleza, aun en su forma más domesticada, servía de recordatorio: los días son ciclos, y cada estación trae su pauta de trabajo y recuperación.

Más tarde, en la periferia, emergió la figura de un grupo de voluntarios que trabajaba en un huerto comunitario. Con guantes y regaderas, removían la tierra y conversaban sobre rotaciones de cultivos, plagas y técnicas naturales para mejorar la salud del suelo. Compartían semillas, herramientas y saberes: generaciones mayores explicaban trucos heredados y jóvenes introducían métodos modernos de compostaje y riego eficiente. El esfuerzo colectivo no sólo alimentaba estómagos, sino que también fortalecía lazos sociales y la sensación de pertenencia.

Al caer la tarde, la ciudad se transformó de nuevo: las luces se encendieron una a una, los cafés se llenaron y la música, que por la mañana había sido un susurro, ahora marcaba el ritmo de encuentros. Ciertas calles se volvieron íntimas; en otras, el bullicio anunciaba festivales improvisados. Las conversaciones se hicieron más profundas, las risas más liberadas y las miradas, a veces, más contemplativas. Cada persona llevaba consigo el día: triunfos, pequeñas derrotas, aprendizajes y la certeza de que mañana habrá otra oportunidad para empezar.

En las horas nocturnas, cuando la calma parecía reinar, se percibía un latido sutil: la ciudad respiraba y, en su respiro, sostenía promesas y proyectos. Había planes que quedaban en papel y otros que ya estaban en marcha; encuentros que cerraban ciclos y otros que sólo comenzaban. La vida urbana mostraba su faceta más compleja, pero también su capacidad para regenerarse, para acoger diferencias y para seguir adelante, un día tras otro.
"""


class Message:

    def _flatten(self, listOfLists: list) -> list:
        """Flatten one level of nesting"""
        return chain.from_iterable(listOfLists)

    def _rBlanks(self, strng: str) -> str:
        """Removes blanks of a string strng and converts to uppercase"""
        return ''.join(strng.split()).upper()

    def _normalize(self, strng: str) -> list:
        """
        Removes blanks spaces of the string 'strng'; then removes accents
        according to 'alphSpecials'. If character 'ñ' occurs in 'strng' then
        'GN' appears in 'accum' as an entry, therefore '_flatten' is needed.
        """
        s = self._rBlanks(strng)
        # s = [ch for ch in self._rBlanks(strng) if ch in alphabet]
        accum = []
        for ch in s:
            if ch in alphSpecials:
                accum.append(alphSpecials[ch])
            else:
                accum.append(ch)
        return filter(lambda x: x in alphabet, self._flatten(accum))
        # return [c for c in self._flatten(accum) if c in alphabet]

    def __init__(self, strng): #constructor los atributos son publicos
        x = self._normalize(strng)
        self.content = ''.join(x)
        self.length = len(self.content)

    def __str__(self):
        return self.content


class Encipher(Message): #la clase Encipher extiende la clase Message, heredando todos los metodos y atributos de Message
    def __init__(self, strng, key):
        Message.__init__(self, strng)
        self.key = key
        self.key_index = 0

    def encoder(self, char):
        return letter_number_map[char]

    def encrypt(self, char):
        encoded_char = self.encoder(char)
        ciphered_num = letter_number_map[self.key[self.key_index]] + encoded_char
        self.key_index = (self.key_index+1)%len(self.key)
        return alphabet[ciphered_num%len(alphabet)]

    def vigenere(self):
        cipher = [self.encrypt(c) for c in self.content]
        return ''.join(cipher)


P = Encipher(message, key)
ciphered_message = P.vigenere()
print(ciphered_message)

class Decipher(): #la clase Encipher extiende la clase Message, heredando todos los metodos y atributos de Message
    def __init__(self, strng, key):
        self.strng = strng
        self.key = key
        self.key_index = 0

    def encoder(self, char):
        return letter_number_map[char]

    def decrypt(self, char):
        encoded_char = self.encoder(char)
        ciphered_num = encoded_char - letter_number_map[self.key[self.key_index]]
        self.key_index = (self.key_index+1)%len(self.key)
        return alphabet[ciphered_num%len(alphabet)]

    def vigenere(self):
        cipher = [self.decrypt(c) for c in self.strng]
        return ''.join(cipher)

Q = Decipher(ciphered_message, 'HOLA')
decipher = Q.vigenere()
print(decipher)