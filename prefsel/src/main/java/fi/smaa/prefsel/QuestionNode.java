package fi.smaa.prefsel;

public class QuestionNode {

	private Question[] remainingQuestions;
	private AnswerNode leftChild;
	private AnswerNode rightChild;
	private Question question;

	/**
	 * Create a question node that has no children expanded.
	 * 
	 * @param question
	 * @param remainingQuestions
	 */
	public QuestionNode(Question question, Question[] remainingQuestions) {
		this.question = question;
		this.remainingQuestions = remainingQuestions;		
	}
	
	public Question getQuestion() {
		return question;
	}
	
	public AnswerNode getLeftChild() {
		return leftChild;
	}
	
	public AnswerNode getRightChild() {
		return rightChild;
	}
	
	public Question[] getRemainingQuestions() {
		return remainingQuestions;
	}
	
	/**
	 * PRECOND: getLeftChild() == null
	 */
	public void expandLeft() {
		if (getLeftChild() != null) {
			throw new IllegalStateException("PRECOND violation: getLeftChild() != null");
		}
		leftChild = new AnswerNode(remainingQuestions, question.getA1());
	}
	
	/**
	 * PRECOND: getRightChild() == null
	 */
	public void expandRight() {
		if (getRightChild() != null) {
			throw new IllegalStateException("PRECOND violation: getRightChild() != null");
		}
		rightChild = new AnswerNode(remainingQuestions, question.getA2());
	}

}
