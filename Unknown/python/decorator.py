"""
Exploring Python Decorators
Author: Andrew Jarombek
Date: 10/12/2018
"""

from datetime import date


def notify(func):
    """
    Decorator function that demonstrates how to add functionality to an existing function.  This
    function prints a message saying that subscribers are being notified of this article.
    :param func: The decorated function
    :return: The decorated function with a wrapper around it
    """

    def _notify(*args):
        print('Notify Subscribers of Article "%s"' % args[0])
        return func(*args)

    return _notify


@notify
def write_article(title, _date):
    """
    A function to create an article.  This function is decorated with the @notify decorator
    :param title: The title of the article
    :param _date: The date of the article
    """
    print('Article "%s" written on %s' % (title, str(_date)))


if __name__ == '__main__':
    write_article('Python Decorators', date(2018, 10, 16))

    # Notify Subscribers of Article "Python Decorators"
    # Article "Python Decorators" written on 2018-10-16
