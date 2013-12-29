package fi.smaa.prefsel;

import static org.junit.Assert.*;

import org.junit.Test;

public class AnswerNodeTest {

	@Test
	public void testChildren() {
		ConcreteAnswerNode a = new ConcreteAnswerNode(4, new Question[]{new Question(1, 2), new Question(2, 3)}, new TransitiveAntisymmetricIrreflexiveRelation(5));
		assertEquals(2, a.getChildren().length);
		assertEquals(5, a.getChildren()[0].getRelation().getDim());
	}
	
	@Test
	public void testRootNode() {
		ConcreteAnswerNode a = new ConcreteAnswerNode(new Question[]{new Question(1, 2), new Question(0, 1)}, 3);
		assertEquals(AnswerNode.NO_ANSWER, a.getAnswer());
	}
}
