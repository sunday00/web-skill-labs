# stack
- FILO
- push, pop
- strict size.
  - dynamic sized value should on Heap.
- assigning is faster than using Heap.

# heap
```mermaid
  sequenceDiagram
      app ->> interface: is there any empty space?

      box rgba(0,0,22,0.4) OS
        participant interface
      end

      interface ->> memory: find empty space then check 'using'  
      
      box rgba(0,22,22,0.4) system
        participant memory
      end
      
      memory ->> interface: pointer
      interface ->> app: pointer
```
- pointer has strict size
  - able to store 'STACK'
  - but value, still in heap.

# calling function
- args, local values are pushed into stack.
- popped at end of function.

# owner
- rust owner related mainly Heap.
- tracking which data are used on heap, minimize duplicate on heap, organize heap

## default rule
- each value has owner
- each value has one owner, default.
- owner is dropped when end of scope. 