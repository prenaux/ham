import unittest

from web_scrape import scrape_python_jobs


class TestWebScrape(unittest.TestCase):
    def test_echo(self) -> None:
        msg = "Hello World"
        self.assertEqual("Hello World", msg)

    def test_scrape_python_jobs(self) -> None:
        result = scrape_python_jobs("https://example.com")

        self.assertEqual(result["Head Title"], "Example Domain")
        self.assertEqual(result["H1 Title"], "Example Domain")
        self.assertEqual(
            result["href 'More Info'"], "https://www.iana.org/domains/example"
        )


if __name__ == "__main__":
    unittest.main()
