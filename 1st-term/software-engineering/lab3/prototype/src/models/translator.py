from models import Order, User, Customer
from models.base import BaseModel
from models.order import OrderView, OrderItemView


class ModelsTranslator:
    """."""

    @classmethod
    def list_to_json(cls, data: list[BaseModel]) -> list[dict]:
        """Translate list of models to list of dicts."""
        return [m.to_dict() for m in data]

    @classmethod
    def order_view(
            cls,
            order: Order,
            customer: Customer | None = None,
            employee: User | None = None,
    ) -> OrderView:
        """."""

        res = OrderView(
            order_id=order.id,
            total_price=order.total_price,
            customer=customer,
            employee=employee,
            created_at=order.created_at,
        )

        for item in order.order_items:
            order_item = OrderItemView(
                name=item.name,
                product_id=item.product_id,
                quantity=item.quantity,
                price=item.price,
                total_price=item.price * item.quantity,
            )
            res.order_items.append(order_item)

        return res
