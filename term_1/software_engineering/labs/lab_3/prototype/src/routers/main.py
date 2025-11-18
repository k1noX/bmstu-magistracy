from flask import Blueprint, render_template, session, redirect, url_for
from flask_login import current_user

main_bp = Blueprint('main', __name__, url_prefix='/')


@main_bp.route('/', methods=['GET'])
def index():
    """."""
    if current_user.is_authenticated:
        return render_template('index.html', user=current_user)
    else:
        return render_template('login.html')


@main_bp.route('/logout/', methods=['GET'])
def logout():
    """Роут для выхода из аккаунта."""
    session.clear()
    return redirect(url_for('main.index'))
