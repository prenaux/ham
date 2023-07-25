from pprint import pformat
from typing import cast, Dict, List, Union

import bs4.element as bs4El
import requests

from bs4 import BeautifulSoup


def scrape_python_jobs(url: Union[bytes, str]) -> Dict[str, Union[List[str], str]]:
    response = requests.get(url)
    soup = BeautifulSoup(response.text, "html.parser")

    el_head = cast(bs4El.Tag, soup.find("head"))
    el_title = cast(bs4El.Tag, el_head.find("title"))
    head_title = el_title.text.strip()

    el_h1 = cast(bs4El.Tag, soup.find("h1"))
    h1_title = el_h1.text.strip()

    attr_href = cast(bs4El.Tag, soup.find("a"))["href"]

    return {
        "Head Title": head_title,
        "H1 Title": h1_title,
        "href 'More Info'": attr_href,
    }


def main() -> None:
    # print("Hello World!")
    url = "https://example.com"
    print(f"I/Scraping: {url}")
    result = scrape_python_jobs(url)
    print(f"I/Result: {pformat(result)}")
    print("I/Done!")


if __name__ == "__main__":
    main()
