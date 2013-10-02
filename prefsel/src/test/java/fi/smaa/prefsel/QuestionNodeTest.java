package fi.smaa.prefsel;

import static org.junit.Assert.*;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class QuestionNodeTest {

	private QuestionNode node;
	private Question[] remQs;

	@Before
	public void setUp() {
		Question q = new Question(1, 2);
		remQs = new Question[]{
				new Question(2, 3),
				new Question(3, 4)
		};
		node = new QuestionNode(q, remQs, new TransitiveAntisymmetricRelation(4));
	}
	
	@Test
	public void testConstructor() {
		Assert.assertArrayEquals(remQs, node.getRemainingQuestions());
		// make sure the children are not expanded
		Assert.assertNull(node.getLeftChild());
		Assert.assertNull(node.getRightChild());
	}
	
	@Test
	public void testExpandLeft() {
		node.expandLeft(remQs, new TransitiveAntisymmetricRelation(3));
		AnswerNode ln = node.getLeftChild();
		assertEquals(1, ln.getAnswer());
	}
	
	@Test
	public void testExpandRight() {
		node.expandRight(remQs, new TransitiveAntisymmetricRelation(3));
		AnswerNode ln = node.getRightChild();
		assertEquals(2, ln.getAnswer());
	}
	
	@Test
	public void testGetChildren() {
		AnswerNode[] c = node.getChildren();
		assertNull(c[0]);
		assertNull(c[1]);
		assertEquals(2, c.length);
	}
}
