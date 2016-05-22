#!/usr/bin/python3

# helpers
''' inverse_int_list: list of int -> list of int '''
iil = lambda lis: [i * -1 for i in lis]

''' string -> list of int '''
s2il = lambda val: [ord(c) for c in val]

''' list of string -> string '''
sl2s = lambda lis: ''.join(lis)

''' list of int -> string '''
il2s = lambda lis: sl2s([chr(i) for i in lis])

''' split_every: string -> list of string '''
se = lambda val, n: [val[i:i+n] for i in range(0, len(val), n)]

''' grow_list: list of int -> list of int '''
gl = lambda lis, n: lis if len(lis) > n else gl(lis + lis, n)

# important
''' make_cipher_list: string, list of int -> list of int '''
mcl = lambda s, k: map(list, zip(s2il(s), k))

''' shift_chars: int, int -> int '''
sc = lambda x, y: x + y if x + y > 0 else 0

''' apply_shift_chars: list of int -> int '''
asc = lambda x: sc(x[0], x[1])

''' cipher: string, list of integer -> string '''
c = lambda s, k: il2s(map(asc, mcl(s, gl(k, len(s)))))

def waterfall_encrypt():
    ''' string, string -> string '''
    return

def waterfall_decrypt():
    ''' string, string -> string '''
    return
