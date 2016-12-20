struct Node {
	int  value;
	Node lchild;
	Node rchild;
  mutex lock;
};

Node dbRoot = createNode(0);

Node createNode(int val) {
  Node node;
  node.lchild = NULL;
  node.rchild = NULL;
  node.value = val;
  node.lock  = mutex_create();
  return node;
}

void destroyNode(Node n) {
  mutex_destroy(n.lock);
  free(n);
}

# returns list with two elements: [target, parent of target]
# assumption: root node is locked by calling thread
list[Node] search(Node root, int val) {
	Node next = NULL;
  Node parent = NULL;
  Node target = NULL;

	if (val < root.value) {
		if ((next = root.lchild) == NULL) {
			target = NULL;
		} else {
			mutex_lock(next.lock);
			if (val == next.value) {
				target = next;
			} else {
				mutex_unlock(root.lock);
				return search(next, val);
			}
		}
	} else {
		if ((next = root.rchild) == NULL) {
			target = NULL;
		} else {
			mutex_lock(next.lock);
			if (val == next.value) {
				target = next;
			} else {
				mutex_unlock(root.lock);
				return search(next, val);
			}
		}
	}

  parent = root;
	return [target, root];
}

int add(Node root, int value)
{
	mutex_lock(root.lock);
  list[Node] result = search(root, value);
  Node target = result[0];
	Node parent = result[1];

  if (target != NULL) {
    if (parent != NULL) {
      mutex_unlock(result[1].lock);
    }
    mutex_unlock(result[0].lock);
    return 0;
  }

  Node newnode = createNode(value);

  if (value < parent.value)
    parent.lchild = newnode;
  else
    parent.rchild = newnode;

  mutex_unlock(parent.lock);
  return 1;
}

void inorderMap(<Node void> f, Node root) {
  if (root == NULL)
    return ;
  inorderMap(f, root.lchild);
  f(root);
  inorderMap(f, root.rchild);
}

struct ThreadArgs {
  int threadNum;
  int from;
  int to;
};

int insertNumbers(ThreadArgs args) {
  int i = args.from;
  for (i = args.from; i < args.to; i += 1) {
    print("thread ");
    printi(args.threadNum);
    add(dbRoot, i);

    # go do something else
    int j = 0;
    int s = 0;
    for (j = 0; j < 100000; j += 1)
      s += j;
  }

  return 0;
}

int main() {
  list[ThreadArgs] argsList = [NULL, NULL, NULL, NULL];

  int i;
  for (i = 0; i < 4; i += 1) {
    ThreadArgs args;
    args.threadNum = i + 1;
    args.from = (i - 2) * 386 + 1;
    args.to = args.from + 20;
    argsList[i] = args;
  }

  parallel(insertNumbers, argsList, 4);

  <Node void> printNode = lambda (Node n) void ( printi(n.value) );
  inorderMap(printNode, dbRoot);
  return 0;
}
