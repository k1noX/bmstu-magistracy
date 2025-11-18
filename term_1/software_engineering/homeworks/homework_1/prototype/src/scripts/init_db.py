import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
sys.path.append(str(project_root))


if __name__ == "__main__":
    from models.database import init_db
    init_db()
    print("База данных успешно инициализирована!")
