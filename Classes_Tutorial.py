# -*- coding: utf-8 -*-
"""
Created on Fri Aug 31 15:45:09 2018

@author: HGospodinov
"""

#Method - a function associated with a class

#e.g. Employee class
class Employee:
    pass

#Class is a blueprint for creating instances
#Every employee with be an instance of the class Employee

#examples
emp_1 = Employee()
emp_2 = Employee()

print(emp_1)
#Instance variables contain data that is unique for each employee

emp_1.first = 'Corey'
emp_1.last = 'Shafer'
emp_1.email = 'Corey_Shafer@something.com'
emp_1.pay = 500

emp_2.first = 'Test'
emp_2.last = 'User'
emp_2.email = 'Testuser@something.com'
emp_2.pay = 50000

#Each instance has attributes unique to them

#we can give them these attributes hwen creating the employee instances

class Employee:
    def __init__(self, first, last, pay):
        self.first =  first
        self.last = last
        self.pay = pay
        self.email = first + '.' + last + '@email.com'
        
    #giving a method to the class
    def fullname(self):
        return(self.first + ' ' + self.last)
        
        
emp_1 = Employee('Corey', 'Shafer', 500)
emp_2 = Employee('Test', 'User', 50000)

emp_1.fullname()


#class variables
class Employee:
    
    raise_amount = 1.05 #class variable
    def __init__(self, first, last, pay):
        self.first =  first
        self.last = last
        self.pay = pay
        self.email = first + '.' + last + '@email.com'
        
    #giving a method to the class
    def fullname(self):
        return(self.first + ' ' + self.last)
        
    def apply_raise(self):
        self.pay = self.pay * Employee.raise_amount
emp_1 = Employee('Corey', 'Shafer', 500)
emp_2 = Employee('Test', 'User', 50000)
        

#if we do this:
emp_1.pay
emp_1.apply_raise()
emp_1.pay
#we can see that we have raise the amount for emp_1
emp_2.pay #without affecting emp_2

#or we can do:
Employee.apply_raise(emp_2)

emp_2.pay #to raise from the class

print(Employee.__dict__)
print(emp_1.__dict__)
#we can see that the raise_amount variable exists only in the class

#accesing this variable through the instance is also possible
print(emp_1.raise_amount) #because it does not exists in the instance, Python look s int eh class
#however if we do
emp_2.raise_amount = 1.10
print(emp_2.raise_amount) #now we create the attribute raise_amount in the instance emp_2

print(emp_2.__dict__) #as we can see here


#important note, the way we ahve defined the raise_amount in the method in the class
# we are taking the self.pay and multiplying by the class variable raise_amount

print(emp_2.pay)
print(emp_2.raise_amount)

print(emp_1.pay)
print(emp_1.raise_amount)

print(Employee.raise_amount)

emp_2.apply_raise()
emp_2.pay #we can see that this raises the emp_2 pay by the variable set in the empoyee
#which is 5% not the instance variable we have created which is 10% 
#to change this behaviour we do the following:
    
    
class Employee:
    
    raise_amount = 1.05 #class variable
    def __init__(self, first, last, pay):
        self.first =  first
        self.last = last
        self.pay = pay
        self.email = first + '.' + last + '@email.com'
        
    #giving a method to the class
    def fullname(self):
        return(self.first + ' ' + self.last)
        
    def apply_raise(self):
        self.pay = self.pay * self.raise_amount
emp_1 = Employee('Corey', 'Shafer', 500)
emp_2 = Employee('Test', 'User', 50000)

print(emp_2.pay)
print(emp_2.raise_amount)

emp_2.raise_amount = 1.1
print(emp_2.raise_amount)

print(emp_1.pay)
print(emp_1.raise_amount)

print(Employee.raise_amount)
        
emp_2.apply_raise()
emp_2.pay



#count of the instances created:
    
class Employee:
    emp_count = 0
    raise_amount = 1.05 #class variable
    def __init__(self, first, last, pay):
        self.first =  first
        self.last = last
        self.pay = pay
        self.email = first + '.' + last + '@email.com'
        Employee.emp_count += 1 #here we def need to use the Class variable instead of self
        
    #giving a method to the class
    def fullname(self):
        return(self.first + ' ' + self.last)
        
    def apply_raise(self):
        self.pay = self.pay * self.raise_amount

emp_1 = Employee('Corey', 'Shafer', 500)
emp_2 = Employee('Test', 'User', 50000)

Employee.emp_count

##class methods

class Employee:
    emp_count = 0
    raise_amount = 1.05 #class variable
    def __init__(self, first, last, pay):
        self.first =  first
        self.last = last
        self.pay = pay
        self.email = first + '.' + last + '@email.com'
        Employee.emp_count += 1 #here we def need to use the Class variable instead of self
        
    #giving a method to the class
    def fullname(self):
        return(self.first + ' ' + self.last)
        
    def apply_raise(self):
        self.pay = self.pay * self.raise_amount
        
        
    @classmethod #this allows us to refer to the class itself(classmethod) 
    def set_raise_amt(cls, amount): #cls is used in a similar fashion as self
        cls.raise_amount = amount
        
        
#then we could do the following:
emp_3 = Employe
        
    
    

emp_1 = Employee('Corey', 'Shafer', 500)
emp_2 = Employee('Test', 'User', 50000)

Employee.set_raise_amt(1.02)
print(Employee.raise_amount)
print(emp_1.raise_amount)
print(emp_2.raise_amount)

#what if we have the cases that someone get his employee info as a string
#and has to parse it before creating an employee instance

emp_new_1 = 'John-Doe-6000'
#one would have to do:
fist,last,sal = emp_new_1.split('-') #before creating the employee
#we could do the following:
    
class Employee:
    emp_count = 0
    raise_amount = 1.05 #class variable
    def __init__(self, first, last, pay):
        self.first =  first
        self.last = last
        self.pay = pay
        self.email = first + '.' + last + '@email.com'
        Employee.emp_count += 1 #here we def need to use the Class variable instead of self
        
    #giving a method to the class
    def fullname(self):
        return(self.first + ' ' + self.last)
        
    def apply_raise(self):
        self.pay = self.pay * self.raise_amount
        
        
    @classmethod #this allows us to refer to the class itself(classmethod) 
    def set_raise_amt(cls, amount): #cls is used in a similar fashion as self
        cls.raise_amount = amount
    @classmethod
    def from_string(cls, emp_string):
        first, last, sal = emp_string.split('-')
        return cls(first,last,sal)
        
#then we could do the following:
    
emp_3 = Employee.from_string(emp_new_1)
emp_3.pay



#inheritance from a class
#say we hav two type of employees - managers/developers

class Developer(Employee):
    pass

dev_1 = Developer('Corey', 'Shafer', 500)
dev_2 = Developer('User', 'Test', 400)
dev_1.apply_raise()
dev_1.pay
print(help(Developer)) #WE CAN SEE THE method resolution order here

#if we want to change the raise amount for developers
class Developer(Employee):
    raise_amount = 1.1
    
dev_1 = Developer('Corey', 'Shafer', 500)
dev_2 = Developer('User', 'Test', 400)
dev_1.apply_raise()
dev_1.pay

#it finds the raise amount in the subclass it changes it only there

#lets say we want to pass in the programming lanaguage of the developer as well

class Developer(Employee):
    
    raise_amt = 1.1
    def __init__(self, first, last, pay, prog_lang):
        super().__init__(first,last,pay) #it's gonna pass it to the parent class to handle it
        # Employee.__init__(self, first,last,pay) #same as above
        self.prog_Lang = prog_lang
        
dev_1 = Developer('Corey', 'Shafer', 500, 'python')
dev_2 = Developer('User', 'Test', 400, 'java')

dev_1.prog_Lang
dev_1.last

#creating a manager class

class Manager_1(Employee):
    
    def __init__(self, first, last, pay, employees=None):
        super().__init__(first,last,pay)
        if employees is None:
            employees = []
        else:
            self.employees = employees
        
    def add_employee(self, emp):
            if emp not in self.employees:
                self.employees.append(emp)
        
    def rem_employee(self, emp):
            if emp  in self.employees:
                self.employees.remove(emp)
        
    def print_emps(self):
            for emp in self.employees:
                print(emp.fullname())
        
#manager_1 = Manager('Sue', 'Smith', 90000, [dev_1])    
mgr_1 = Manager_1('Someone', 'Else', 20000, [dev_1])
mgr_1.add_employee(dev_2)
mgr_2 = Manager_1('Someone', 'Else', 20000, [dev_2])
print(manager_1.email)      
print(mgr_1.print_emps())

print(isinstance(mgr_1, Manager_1))
print(isinstance(mgr_1, Employee))
print(isinstance(mgr_1, Developer))

print(issubclass(Manager_1, Developer))
print(issubclass(Developer, Employee))
print(issubclass(Manager_1, Employee))


 