from typing import List, Generic, TypeVar, Optional, Tuple, Set
import sys

T = TypeVar('T')

class Tree(Generic[T]):
    def flatten_paths(self) -> List[List[T]]:
        raise NotImplementedError

    def draw_tree(self) -> str:
        visited: Set[int] = set()
        
        def build_chain(t: 'Tree[T]') -> Tuple[str, List['Tree[T]']]:
            curr = t
            chain: List[str] = []
            branches: List['Tree[T]'] = []
            stop = False
            
            while not stop:
                obj_id = id(curr)
                
                if obj_id in visited:
                    if chain:
                        chain_str = " -> ".join(chain) + " -> "
                    else:
                        chain_str = ""
                    return (f"{chain_str}(ref: @{hex(obj_id)})", [])
                
                visited.add(obj_id)
                
                if isinstance(curr, DeadEnd):
                    chain.append("DeadEnd")
                    stop = True
                elif isinstance(curr, Node):
                    # Node
                    vals = ", ".join(str(v) for v in curr.values)
                    chain.append(f"{vals} @{hex(obj_id)}")
                    
                    if not curr.branches:
                        stop = True
                    elif len(curr.branches) == 1:
                        curr = curr.branches[0]
                    else:
                        branches = curr.branches
                        stop = True
                else:
                    stop = True

            return (" -> ".join(chain), branches)

        def loop(t: 'Tree[T]', prefix: str, is_last: bool, is_root: bool) -> str:
            chain_str, branches = build_chain(t)
            
            marker = ""
            child_indent = ""
            if is_root:
                marker = ""
                child_indent = ""
            elif is_last:
                marker = "└── "
                child_indent = "    "
            else:
                marker = "├── "
                child_indent = "│   "
            
            header = f"{prefix}{marker}{chain_str}"
            
            if not branches:
                return header + "\n"
            else:
                body = ""
                for i, b in enumerate(branches):
                    body += loop(b, prefix + child_indent, i == len(branches) - 1, False)
                return header + "\n" + body
        
        # Reset visited for each draw call if we want to allow re-drawing
        # But keeping it local to draw_tree is correct.
        return loop(self, "", True, True)

class Node(Tree[T]):
    def __init__(self, values: List[T], branches: List['Tree[T]']):
        self.values = values
        self.branches = branches

    def flatten_paths(self) -> List[List[T]]:
        if not self.branches:
            return [list(self.values)]
        
        prefix = list(self.values)
        paths = []
        for b in self.branches:
            for p in b.flatten_paths():
                paths.append(prefix + p)
        return paths

class DeadEnd(Tree[T]):
    def flatten_paths(self) -> List[List[T]]:
        return []

# Helper functions
def leaf(value: T) -> Tree[T]:
    return Node([value], [])

def fork(branches: List[Tree[T]]) -> Tree[T]:
    return Node([], branches)

def single(value: T, next_node: Tree[T]) -> Tree[T]:
    return Node([value], [next_node])
