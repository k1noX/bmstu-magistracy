from .base import BaseModel


class ModuleException(Exception, BaseModel):
    """."""
    status_code = 500
    base_message = 'Ошибка'

    def __init__(
            self,
            message: str | None = None,
            status_code: int | None = None,
            data: dict | None = None,
    ):
        super().__init__(f'{self.base_message}: {message or ""}')
        self._status_code = status_code or self.status_code
        self._data = data or dict()

    def to_dict(self) -> dict:
        """Преобразование ошибки в словарь для JSON-ответа."""
        return dict(
            error=self.__class__.__name__,
            message=str(self),
            data=self._data,
        )


class UserModuleException(ModuleException):
    """Базовый класс исключений для модуля пользователей."""
    base_message = "Ошибка в модуле пользователей"


class UserValidationError(UserModuleException):
    """Ошибка валидации пользователя."""
    status_code = 401
    base_message = 'Ошибка валидации'

    def __init__(self, reason: str, data: dict | None = None):
        super().__init__(
            message=reason,
            data=data or {"reason": reason},
        )


class UserAuthorizationError(UserModuleException):
    """Ошибка авторизации пользователя."""
    status_code = 401

    def __init__(
            self,
            username: str | None = None,
            data: dict | None = None,
    ):
        super().__init__(
            message=f"ошибка авторизации.",
            data=data or {"username": username},
        )


class UserNotFoundError(UserModuleException):
    """Ошибка, возникающая при отсутствии пользователя."""
    status_code = 404

    def __init__(
            self,
            username: str | None = None,
            user_id: int | None = None,
            data: dict | None = None,
    ):
        super().__init__(
            message=f"пользователь не найден.",
            data=data or {"username": username, "user_id": user_id},
        )


class OrderModuleException(ModuleException):
    """Базовый класс исключений для модуля заказов."""
    base_message = "Ошибка в модуле заказов"


class OrderNotFoundError(OrderModuleException):
    """Исключение: заказ не найден."""
    status_code = 404

    def __init__(self, order_id: int, data: dict | None = None):
        super().__init__(
            message=f"заказ с ID {order_id} не найден.",
            data=data or {"order_id": order_id},
        )


class CustomerNotFoundError(ModuleException):
    """Исключение: клиент не найден."""
    status_code = 404

    def __init__(self, customer_id: int, data: dict | None = None):
        super().__init__(
            message=f"клиент с ID {customer_id} не найден.",
            data=data or {"customer_id": customer_id},
        )


class ProductNotFoundError(ModuleException):
    """Исключение: продукт не найден."""
    status_code = 404

    def __init__(self, product_id: int, data: dict | None = None):
        super().__init__(
            message=f"продукт с ID {product_id} не найден.",
            data=data or {"product_id": product_id},
        )


class InvalidOrderError(OrderModuleException):
    """Исключение: данные заказа некорректны."""
    status_code = 400

    def __init__(self, reason: str, data: dict | None = None):
        super().__init__(
            message=f"некорректные данные заказа: {reason}.",
            data=data or {"reason": reason},
        )


class OrderDeletionError(OrderModuleException):
    """Исключение: невозможно удалить заказ."""
    status_code = 400

    def __init__(
            self,
            order_id: int,
            reason: str | None = None,
            data: dict | None = None,
    ):
        super().__init__(
            message=f"не удалось удалить заказ с ID {order_id}.",
            data=data or {"order_id": order_id, "reason": reason},
        )


class ProductDeletionError(ModuleException):
    """Исключение: невозможно удалить заказ."""
    status_code = 400

    def __init__(
            self,
            product_id: int,
            reason: str | None = None,
            data: dict | None = None,
    ):
        super().__init__(
            message=f"не удалось удалить товар с ID {product_id}.",
            data=data or {"order_id": product_id, "reason": reason},
        )


class CustomerDeletionError(ModuleException):
    """Исключение: невозможно удалить клиента."""
    status_code = 400

    def __init__(
        self,
        customer_id: int,
        reason: str | None = None,
        data: dict | None = None,
    ):
        super().__init__(
            message=f"не удалось удалить клиента с ID {customer_id}.",
            data=data or {"order_id": customer_id, "reason": reason},
        )


class OrderCreationError(ModuleException):
    """Исключение: невозможно создать заказ."""
    status_code = 400

    def __init__(
            self,
            reason: str | None = None,
            data: dict | None = None,
    ):
        super().__init__(
            message=f"не удалось создать товар.",
            data=data or {"reason": reason},
        )


class ProductCreationError(ModuleException):
    """Исключение: невозможно создать товар."""
    status_code = 400

    def __init__(
            self,
            reason: str | None = None,
            data: dict | None = None,
    ):
        super().__init__(
            message=f"не удалось создать товар.",
            data=data or {"reason": reason},
        )


class CustomerCreationError(ModuleException):
    """Исключение: невозможно создать клиента."""
    status_code = 400

    def __init__(
        self,
        reason: str | None = None,
        data: dict | None = None,
    ):
        super().__init__(
            message=f"не удалось создать клиента.",
            data=data or {"reason": reason},
        )
