def extract_lines_from_std(buffer) -> list:
    """
    Extract lines of buffer by split "\n" without the empty last line
    :param buffer:
    :return: list of lines
    """

    lines_wla = buffer.split('\n')
    lines_sanitize = lines_wla[:-1] if len(lines_wla[-1]) == 0 else lines_wla
    return list(map(lambda buff: buff + "\n", lines_sanitize))


def last(l: list) -> object:
    """
    Get the last element
    :param l:
    :return: last item of the list if len > 0 else None
    """
    return l[-1] if len(l) > 0 else None
