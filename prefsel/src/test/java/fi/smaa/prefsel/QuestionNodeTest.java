package fi.smaa.prefsel;

import static org.junit.Assert.*;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
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
		double[][] vals = new double[][]{
				{0.0, 1.0, 2.0},
				{1.0, 0.0, 2.0},
				{0.0, 3.0, 0.2},
				{2.0, 4.0, 0.1},
				{3.0, 1.0, 2.2}
		};
		node = new QuestionNode(q, remQs, new TransitiveRelation(5), new NullPreferenceModel(), new Array2DRowRealMatrix(vals));
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
		node.expandLeft();
		AnswerNode ln = node.getLeftChild();
		assertEquals(1, ln.getAnswer());
	}
	
	@Test
	public void testExpandRight() {
		node.expandRight();
		AnswerNode ln = node.getRightChild();
		assertEquals(2, ln.getAnswer());
	}

}
