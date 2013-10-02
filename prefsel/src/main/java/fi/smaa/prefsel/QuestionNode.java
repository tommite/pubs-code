package fi.smaa.prefsel;


public class QuestionNode implements Node<QuestionNode, AnswerNode> {

	private TransitiveAntisymmetricRelation prefs;
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
	public QuestionNode(Question question, Question[] remainingQuestions, TransitiveAntisymmetricRelation prefs) {
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

	/*
	private AnswerNode createAnswerNode(int a1, int a2) {
		if (a1 < 0 || a2 < 0 || a1 >= impacts.getRowDimension() || a2 >= impacts.getRowDimension()) {
			throw new IllegalArgumentException("PRECOND violation");
		}
		
		TransitiveRelation newPrefs = prefs.deepCopy();
		PreferenceModel newPrefModel = prefModel.copyWithRelation(newPrefs);
		newPrefs.addRelation(a1, a2);
		ArrayList<Question> newQ = new ArrayList<Question>();
		
		for (Question q : remainingQuestions){
			System.out.println("answer " + a1 + " > " + a2 + " q: " + q);
			PreferenceRelation compRes = newPrefModel.compare(impacts.getRow(q.getA1()), impacts.getRow(q.getA2()));
			if (compRes == PreferenceRelation.FIRST_PREFERRED) {
				newPrefs.addRelation(a1, a2);
				System.out.println("first");
			} else if (compRes == PreferenceRelation.SECOND_PREFERRED) {
				newPrefs.addRelation(a2, a1);
				System.out.println("second");
			} else {
				if (!newPrefs.getRelation(q.getA1(), q.getA2()) &&
						!newPrefs.getRelation(q.getA2(), q.getA1())) {
					newQ.add(q);
				}
			}
		}
		return new AnswerNode(newQ.toArray(new Question[0]), a1, newPrefs, newPrefModel, impacts);
	}
	*/

	
	/**
	 * PRECOND: getLeftChild() == null
	 */
	public void expandLeft(Question[] remainingQs, TransitiveAntisymmetricRelation newPrefs) {
		if (getLeftChild() != null) {
			throw new IllegalStateException("PRECOND violation: getLeftChild() != null");
		}
		leftChild = new AnswerNode(question.getA1(), remainingQs, newPrefs);
	}
	
	/**
	 * PRECOND: getRightChild() == null
	 */
	public void expandRight(Question[] remainingQs, TransitiveAntisymmetricRelation newPrefs) {
		if (getRightChild() != null) {
			throw new IllegalStateException("PRECOND violation: getRightChild() != null");
		}
		rightChild = new AnswerNode(question.getA2(), remainingQs, newPrefs);
	}
	
	
	public AnswerNode[] getChildren() {
		return new AnswerNode[] {getLeftChild(), getRightChild()};
	}
	
	@Override
	public String toString() {
		return "q" + getLeftChild().getAnswer() + "or" + getRightChild().getAnswer();
	}

	public TransitiveAntisymmetricRelation getRelation() {
		return prefs;
	}
}
