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
		node = new QuestionNode(q, remQs, new TransitiveAntisymmetricIrreflexiveRelation(4));
	}
	
	@Test
	public void testConstructor() {
		Assert.assertArrayEquals(remQs, node.getRemainingQuestions());
		// make sure the children are not expanded
		Assert.assertTrue(node.getLeftChild() instanceof UnexpandedNode);
		Assert.assertTrue(node.getRightChild() instanceof UnexpandedNode);
	}
	
	@Test
	public void testExpandLeft() {
		node.expandLeft(remQs, new TransitiveAntisymmetricIrreflexiveRelation(3));
		AnswerNode ln = node.getLeftChild();
		assertEquals(1, ln.getAnswer());
	}
	
	@Test
	public void testExpandRight() {
		node.expandRight(remQs, new TransitiveAntisymmetricIrreflexiveRelation(3));
		AnswerNode ln = node.getRightChild();
		assertEquals(2, ln.getAnswer());
	}
	
	@Test
	public void testGetChildren() {
		AnswerNode[] c = node.getChildren();
		assertEquals(2, c.length);
		assertTrue(c[0] instanceof UnexpandedNode);
		assertTrue(c[1] instanceof UnexpandedNode);
	}
}
