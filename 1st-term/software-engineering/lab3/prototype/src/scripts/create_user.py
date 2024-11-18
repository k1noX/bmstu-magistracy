import sys
from pathlib import Path
import argparse

project_root = Path(__file__).resolve().parent.parent
sys.path.append(str(project_root))


if __name__ == "__main__":
    from injectors.services import ServicesInjector

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-u', '--username',
        required=True,
    )
    parser.add_argument(
        '-p', '--password',
        required=True,
    )
    args = parser.parse_args()
    ServicesInjector().users().add(args.username, args.password)
    print("Пользователь успешно создан!")
