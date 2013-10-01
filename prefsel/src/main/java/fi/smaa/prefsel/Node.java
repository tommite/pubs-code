package fi.smaa.prefsel;

public interface Node<T, C> {

	public Node<C, T>[] getChildren();
}
