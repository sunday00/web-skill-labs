local BankAccount = {
    isOpen = false,
    amount = 0,
}

function BankAccount:new()
    self.isOpen = false

    return self
end

function BankAccount:open()
    if self.isOpen == true then
        error()
    end

    self.amount = 0
    self.isOpen = true
end

function BankAccount:close()
    if self.isOpen ~= true then
        error()
    end

    self.amount = 0
    self.isOpen = false
end

function BankAccount:deposit(amt)
    if self.isOpen ~= true or amt < 0 then
        error()
    end

    self.amount = self.amount + amt
end

function BankAccount:withdraw(amt)
    if self.isOpen ~= true or amt < 0 then
        error()
    end

    self.amount = self.amount - amt

    if self.amount < 0 then
        error()
    end
end

function BankAccount:balance()
    if self.isOpen ~= true then
        error()
    end

    return self.amount
end

--local account = BankAccount:new()
--account:open()
--account:deposit(50)
--account:close()
--account:open()
--print(account:balance())

return BankAccount
