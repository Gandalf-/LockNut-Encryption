#!/usr/bin/env python3

import unittest


class algorithm(unittest.TestCase):

    def test_encrypt(self):

        message = ["a", "b", "c"]
        values = map(ord, message)
        key = [1] + values[:-1]
        output = []

        for i in range(0, len(values)):
            output += [values[i] ^ key[i]]

        self.assertEqual(output, [96, 3, 1])

    def test_encrypt_chunk(self):

        message = ["ab", "bc", "de"]
        values = [map(ord, chunk) for chunk in message]
        key = [[1, 2]] + values[:-1]
        output = []

        # for each chunk in the input
        for i in range(0, len(values)):
            temp = []

            # xor each pair of characters
            for j in range(0, len(values[i])):
                temp += [values[i][j] ^ key[i][j]]

            output += [temp]

        self.assertEqual(
            output, [[96, 96], [3, 1], [6, 6]])

    def test_decrypt(self):

        values = [96, 3, 1][::-1]
        key = 1
        output = []

        while values:
            head = values.pop()
            result = key ^ head
            output += [result]
            key = result

        self.assertEqual(output, [97, 98, 99])


if __name__ == '__main__':
    unittest.main()
