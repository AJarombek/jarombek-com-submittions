# Author: Andrew Jarombek
# Date: 1/18/2018
# Credentials for the RabbitMQ broker


class Cred(object):

    def __init__(self, username):

        self.__server = 'localhost'
        self.__vhost = '/'
        self.__exchange = 'running-log'

        if username == 'consumer':
            self.__username = 'running-log-consumer'
            self.__password = 'iconsume'
        elif username == 'producer':
            self.__username = 'running-log-producer'
            self.__password = 'iproduce'
        else:
            self.__username = ''
            self.__password = ''

    # Expose these properties and make them immutable
    @property
    def SERVER(self):
        return self.__server

    @SERVER.setter
    def SERVER(self, value):
        raise ValueError("Unable to set a value to SERVER")

    @property
    def VHOST(self):
        return self.__vhost

    @VHOST.setter
    def VHOST(self, value):
        raise ValueError("Unable to set a value to VHOST")

    @property
    def EXCHANGE(self):
        return self.__exchange

    @EXCHANGE.setter
    def EXCHANGE(self, value):
        raise ValueError("Unable to set a value to EXCHANGE")

    @property
    def USERNAME(self):
        return self.__username

    @USERNAME.setter
    def USERNAME(self, value):
        raise ValueError("Unable to set a value to USERNAME")

    @property
    def PASSWORD(self):
        return self.__password

    @PASSWORD.setter
    def PASSWORD(self, value):
        raise ValueError("Unable to set a value to PASSWORD")
