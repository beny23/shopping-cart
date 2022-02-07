# shopping-cart Haskell solution

Contains the code for a naive (i.e. I've only recently started learning) Haskell solution to the shopping cart problem.

It was developed using stack.  Run the tests as follows:

```shell
$ stack runhaskell *Spec.hs

empty
  should create an empty cart
add
  should add 2 cornflakes
  should combine the same product
subTotal
  should correctly calculate cornflakes
roundCents
  should correctly round up
  should correctly round down
  should correctly round .5 up
tax
  should correctly tax cornflakes
  should correctly calculate no tax
  should correctly round the tax
total
  should correctly calculate total with no tax
  should correctly calculate total with tax
  should correctly calculate carts with multiple items
Part 1
  should match the expected input

Finished in 0.0033 seconds
14 examples, 0 failures
```