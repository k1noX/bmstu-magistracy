import time
from locust import HttpUser, task, between


class QuickstartUser(HttpUser):
    wait_time = between(1, 5)

    @task(3)
    def view_orders(self):
        self.client.get("/orders/")

    @task(3)
    def view_products(self):
        self.client.get("/products/")

    @task(3)
    def view_customers(self):
        self.client.get(f"/customers/")

    @task(4)
    def create_customer(self):
        self.client.post(
            "/customers/",
            json=dict(
                fullname='test',
                email='test email',
                phone='88005553535',
            )
        )

    @task(3)
    def create_product(self):
        self.client.post(
            "/products/",
            json=dict(
                name='test',
                price=10.0,
                description='stress testing',
            )
        )

    @task(3)
    def create_order(self):
        self.client.post(
            "/orders/",
            json=dict(
                items=[
                    dict(
                        product_id=2,
                        quantity=100,
                    ),
                ],
                customer_id=1,
            )
        )

    def on_start(self):
        self.client.post("/users/", json={"username":"admin", "password":"121212aa"})
