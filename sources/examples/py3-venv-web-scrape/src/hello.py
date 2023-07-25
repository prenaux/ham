import argparse


def hello(aName: str, upper: bool = False) -> str:
    if upper:
        return "Hello " + aName.upper() + "!"
    else:
        return "Hello " + aName + "!"


def main() -> None:
    parser = argparse.ArgumentParser(description="Say hello.")
    parser.add_argument(
        "name", nargs="?", default="ham", type=str, help="the name to greet"
    )
    parser.add_argument(
        "-u", "--uppercase", action="store_true", help="uppercase the greeting"
    )

    args = parser.parse_args()

    greeting = hello(args.name, args.uppercase)
    print(greeting)


if __name__ == "__main__":
    main()
