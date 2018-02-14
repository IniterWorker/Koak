def format_error(row: int, col: int, err: str):
    """
    Generate koak error format (stderr of koak binary)
    :param row:
    :param col:
    :param err:
    :return: str
    """
    return "Syntax Error at line {}, column {}: {}\n".format(row, col, err)