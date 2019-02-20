"""
Experimenting with Different Unicode encodings
Author: Andrew Jarombek
Date: 10/2/2018
"""

from unicodedata import normalize

# Two different unicode representations of 'beyoncé'
b = 'beyonce\u0301'
b2 = 'beyonc\u00E9'

print(b)   # beyoncé
print(b2)  # beyoncé

# While both representations appear the same, they are not considered equal
assert not b == b2

# The UTF-8 byte encodings.  \xcc\x81 is equivalent to (U+0301), the unicode code point for
# a 'combining acute accent'
assert b.encode('utf8') == b'beyonce\xcc\x81'

# \xc3\xa9 is equivalent to (U+00E9), the unicode code point for 'é'
assert b2.encode('utf8') == b'beyonc\xc3\xa9'

# The UTF-16 encoding for 'beyonce\u0301'.  Each character is at least 2 bytes long, compared to
# UTF-8 where characters that are compatible with ASCII are encoded with 1 byte.
# Therefore 'b' is encoded as 'b\x00' in UTF-16 vs 'b' in UTF-8.
# The first two bytes (\xff\xfe) are the BOM, which determines whether UTF-16 is stored as
# little endian (less significant bytes first) or big endian (more significant bytes first).
# \xff\xfe specifies little endian.  UTF-8 is endianless, so the BOM is usually omitted.
assert b.encode('utf-16') == b'\xff\xfeb\x00e\x00y\x00o\x00n\x00c\x00e\x00\x01\x03'

# The UTF-32 encoding for 'beyonce\u0301'.
assert b.encode('utf-32') == b'\xff\xfe\x00\x00b\x00\x00\x00e\x00\x00\x00y\x00\x00\x00' \
                             b'o\x00\x00\x00n\x00\x00\x00c\x00\x00\x00e\x00\x00\x00\x01\x03\x00\x00'

# Both representations have different lengths.
# 'beyoncé' appears as length 7, however variable 'b' resolves to length 8
assert len(b) == 8
assert len(b2) == 7

# Normalize both strings using the form NFC
normalized_b = normalize('NFC', b)
normalized_b2 = normalize('NFC', b2)

assert len(normalized_b) == 7
assert len(normalized_b2) == 7

assert normalized_b == normalized_b2

smiley = '😊'

assert len(smiley) == 1
assert smiley.encode('UTF-8') == b'\xf0\x9f\x98\x8a'
assert smiley.encode('UTF-16') == b'\xff\xfe=\xd8\n\xde'