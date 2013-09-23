package fi.smaa.prefsel;

import java.util.ArrayList;

public class QuestionNode implements Node {

	private Question[] remainingQuestions;
	private AnswerNode leftChild;
	private AnswerNode rightChild;
	private Question question;
	private TransitiveRelation prefs;

	/**
	 * Create a question node that has no children expanded.
	 * 
	 * @param question
	 * @param remainingQuestions
	 */
	public QuestionNode(Question question, Question[] remainingQuestions, TransitiveRelation prefs) {
		this.question = question;
		this.remainingQuestions = remainingQuestions;		
		this.prefs = prefs;
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

	private AnswerNode createAnswerNode(int a1, int a2) {
		TransitiveRelation newPrefs = prefs.deepCopy();
		newPrefs.addRelation(a1, a2);
		ArrayList<Question> newQ = new ArrayList<Question>();
		for (Question q : remainingQuestions){
			if (!newPrefs.getRelation(q.getA1(), q.getA2()) &&
					!newPrefs.getRelation(q.getA2(), q.getA1())) {
				newQ.add(q);
			}
		}
		return new AnswerNode(newQ.toArray(new Question[0]), a1, newPrefs);
	}

	/**
	 * PRECOND: getLeftChild() == null
	 */
	public void expandLeft() {
		if (getLeftChild() != null) {
			throw new IllegalStateException("PRECOND violation: getLeftChild() != null");
		}
		leftChild = createAnswerNode(question.getA1(), question.getA2());
	}
	
	/**
	 * PRECOND: getRightChild() == null
	 */
	public void expandRight() {
		if (getRightChild() != null) {
			throw new IllegalStateException("PRECOND violation: getRightChild() != null");
		}
		rightChild = createAnswerNode(question.getA2(), question.getA1());
	}
	
	public AnswerNode[] getChildren() {
		return new AnswerNode[] {getLeftChild(), getRightChild()};
	}
	
	@Override
	public String toString() {
		return "q" + getLeftChild().getAnswer() + "or" + getRightChild().getAnswer();
	}
	

}
