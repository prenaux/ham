import unittest

from hello import hello


class TestHello(unittest.TestCase):
    def test_hello(self) -> None:
        msg = hello("World")
        self.assertEqual("Hello World!", msg)

    def test_hello_upper(self) -> None:
        msg = hello("World", True)
        self.assertEqual("Hello WORLD!", msg)


if __name__ == "__main__":
    unittest.main()
